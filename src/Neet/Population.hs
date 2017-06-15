{-
Copyright (C) 2015 Leon Medvinsky

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 3
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
-}

{-|
Module      : Neet.Population
Description : Population of NEAT organisms
Copyright   : (c) Leon Medvinsky, 2015

License     : GPL-3
Maintainer  : lmedvinsky@hotmail.com
Stability   : experimental
Portability : ghc
-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
module Neet.Population (
                         Population(..)
                       , SpecId(..)
                         -- * PopM
                       , PopM
                       , PopContext
                       , runPopM
                         -- * Construction
                       , PopSettings(..)
                       , newPop
                         -- * Training
                       , trainOnce
                       , TrainMethod(..)
                         -- ** TrainMethods
                       , pureTrain
                       , winTrain
                       , trainTrain
                         -- ** Convenience
                       , trainN
                       , trainUntil
                       , trainPure
                         -- * Statistics
                       , speciesCount
                         -- * Debugging
                       , validatePopulation
                       ) where

import Neet.Species
import Neet.Genome
import Neet.Parameters
import Neet.Training

import Data.MultiMap (MultiMap)

import qualified Data.MultiMap as MM

import Data.Map (Map)
import qualified Data.Map as M

import Data.List (foldl', maximumBy, sortBy)

import Data.Maybe
import Data.Monoid

import Data.Functor.Identity

import Control.Monad.Random
import Control.Monad.Fresh.Class
import Control.Monad.Trans.State

import Control.Monad

import Data.Traversable

import Control.Parallel.Strategies

import Data.Function

newtype SpecId = SpecId Int
               deriving (Show, Eq, Ord)


-- | A NEAT Population
data Population =
  Population { popSize   :: Int                -- ^ Size of the population
             , popSpecs  :: !(Map SpecId Species) -- ^ The species
             , popBScore :: !Double             -- ^ Best score so far
             , popBOrg   :: !Genome             -- ^ Best genome so far
             , popBSpec  :: !SpecId             -- ^ Id of the species that hosted the best score
             , popCont   :: !PopContext         -- ^ Tracking state and fresh values
             , nextSpec  :: !SpecId             -- ^ The next species ID
             , popParams :: Parameters        -- ^ Parameters for large species
             , popStrat  :: SearchStrat
             , popPhase  :: PhaseState
             , popGen    :: Int                -- ^ Current generation
             }
  deriving (Show)


-- | Describes how to handle each species or genome when processing fitness,
-- allowing the addition of additional effects
newtype TrainMethod f =
  TrainMethod { tmGen :: forall t. Traversable t => t Genome -> f (t Double)
                -- ^ How to process each 'Genome' of a species into a score.
              }


-- | Train method without any additional effects
pureTrain :: GenScorer a -> TrainMethod Identity
pureTrain gs = TrainMethod go
  where go gmap = Identity $ fmap (fitnessFunction gs . gScorer gs) gmap


-- | Train method that possibly returns a solution
winTrain :: GenScorer a -> TrainMethod ((,) (First Genome))
winTrain gs = TrainMethod (traverse go)
  where go genome
          | winCriteria gs score = (First (Just genome), fitnessFunction gs score)
          | otherwise = (First Nothing, fitnessFunction gs score)
          where score = gScorer gs genome


-- | Train method that gives a 'Training'.
trainTrain :: TrainMethod (Training Genome Double)
trainTrain = TrainMethod trainTraversable


data PopContext =
  PC { nextInno :: InnoId
     , randGen  :: StdGen
     } 
  deriving (Show)

-- | Custom state monad
newtype PopM a = PopM (State PopContext a)
            deriving (Functor, Applicative, Monad)


instance MonadRandom PopM where
  getRandom = PopM . state $ \s ->
    let (r, gen) = random (randGen s)
    in (r, s { randGen = gen })
  getRandoms = PopM . state $ \s ->
    let (g1, g2) = split $ randGen s
    in (randoms g1, s { randGen = g2 })
  getRandomR range = PopM . state $ \s ->
    let (r, gen) = randomR range (randGen s)
    in (r, s { randGen = gen })
  getRandomRs range = PopM . state $ \s ->
    let (g1, g2) = split $ randGen s
    in (randomRs range g1, s { randGen = g2 })


instance MonadFresh InnoId PopM where
  fresh = PopM . state $ \s ->
    let inno@(InnoId x) = nextInno s
    in (inno, s { nextInno = InnoId $ x + 1 })


runPopM :: PopM a -> PopContext -> (a, PopContext)
runPopM (PopM ma) = runState ma


-- | Settings for creating a new population
data PopSettings =
  PS { psSize    :: Int        -- ^ How big the population should be
     , psInputs  :: Int        -- ^ Number of inputs
     , psOutputs :: Int        -- ^ Number of outputs
     , psParams  :: Parameters -- ^ Parameters for large species
     , sparse    :: Maybe Int  -- ^ If Just n, will be sparse with n connections.
                               -- Otherwise fully connected.
     , psStrategy :: Maybe PhaseParams
     } 
  deriving (Show)


newtype SpecM a = SM (State SpecId a)
                deriving (Functor, Applicative, Monad)


instance MonadFresh SpecId SpecM where
  fresh = SM . state $ \s@(SpecId x) -> (s, SpecId $ x + 1)


runSpecM :: SpecM a -> SpecId -> (a, SpecId)
runSpecM (SM ma) = runState ma


-- | Buckets for speciation
data SpecBucket =
  SB SpecId Genome [Genome]


-- | Speciation helper
shuttleOrgs :: MonadFresh SpecId m =>
               Parameters -> [SpecBucket] -> [Genome] -> m [SpecBucket]
shuttleOrgs p@Parameters{..} buckets = foldM shutOne buckets
  where DistParams{..} = distParams specParams
        shutOne :: MonadFresh SpecId m => [SpecBucket] -> Genome -> m [SpecBucket]
        shutOne (SB sId rep gs:bs) g
          | distance p g rep <= delta_t = return $ SB sId rep (g:gs) : bs
          | otherwise = liftM (SB sId rep gs :) $ shutOne bs g
        shutOne [] g = do
          newId <- fresh
          return $ [SB newId g [g]]


zipWithDefaults :: (a -> b -> Maybe c) -> (a -> Maybe c) -> (b -> Maybe c) -> [a] -> [b] -> [c]
zipWithDefaults _ _  db [] bs = mapMaybe db bs
zipWithDefaults _ da _  as [] = mapMaybe da as
zipWithDefaults f da db (a:as) (b:bs) =
  case f a b of
   Just res -> res : zipWithDefaults f da db as bs
   Nothing -> zipWithDefaults f da db as bs


-- | Speciation function
speciate :: MonadFresh SpecId m =>
            Parameters -> Map SpecId Species -> [Genome] -> m (Map SpecId Species)
speciate params specs gens = do
  filled <- fill
  let zipped = zipWithDefaults mkSpecies (const Nothing) newSpecies specL filled
  return $ M.fromList zipped
  where oneBucket (k, Species _ (rep:_) _ _) = SB k rep []
        oneBucket _                          = error "(speciate) Empty species!"
        assocs = M.toList specs
        specL = map snd assocs
        buckets = map oneBucket assocs
        fill = shuttleOrgs params buckets gens
        mkSpecies (Species _ _ scr imp) (SB sId _ gs)
          | null gs = Nothing
          | otherwise = Just $ (sId, Species (length gs) gs scr imp)
        newSpecies (SB _ _ []) = Nothing
        newSpecies (SB sId _ (g:gs)) = Just $ (sId, newSpec g gs)


-- | Generates a starter population
newPop :: Int -> PopSettings -> Population
newPop seed PS{..} = fst $ runPopM generate initCont
  where Parameters{..} = psParams
        popSize = psSize
        popBScore = 0
        popBSpec = SpecId 1
        initCont = PC (InnoId $ psInputs * psOutputs + 2) (mkStdGen seed)
        popParams = psParams
        orgGenner = case sparse of
                     Nothing -> fullConn mutParams
                     Just conCount -> sparseConn mutParams conCount
        generateGens = replicateM psSize (orgGenner psInputs psOutputs)
        popGen = 1
        generate = do
          gens <- generateGens
          let (popSpecs, nextSpec) = runSpecM (speciate psParams M.empty gens) (SpecId 1)
              popBOrg = head gens
              avgComp = fromIntegral (foldl' (+) 0 . map genomeComplexity $ gens) / fromIntegral popSize
              (popStrat, popPhase) = case psStrategy of
                Nothing -> (Complexify, Complexifying 0) -- not used
                Just pp@PhaseParams{..} ->
                  (Phased pp, Complexifying (phaseAddAmount + avgComp))
          popCont <- PopM get
          return Population{..}


trainOnce :: Applicative f => TrainMethod f -> Population -> f Population
trainOnce method pop = fmap (flip trainOnceWFits pop) $ specRes
  where specRes = traverse (runFitTestWStrategy (tmGen method)) (popSpecs pop)


-- | Advances the population one generation with the fitness function, possibly
-- giving a solution.
trainOnceWFits :: Map SpecId TestResult -> Population -> Population
trainOnceWFits tResults pop = generated
  where params = popParams pop

        mParams = mutParams params
        mParamsS = mutParamsS params

        avgComp = avgComplexity pop

        newPhase = case (popPhase pop, popStrat pop) of
          (_, Complexify) -> Complexifying 0
          (Complexifying thresh, Phased PhaseParams{..})
            | avgComp < thresh -> Complexifying thresh
            | otherwise -> Pruning 0 avgComp
          (Pruning lastFall lastComp, Phased PhaseParams{..})
            | avgComp < lastComp -> Pruning 0 avgComp
            | lastFall >= phaseWaitTime -> Complexifying (avgComp + phaseAddAmount)
            | otherwise -> Pruning (lastFall + 1) avgComp
            
        isPruning = case newPhase of
          Pruning _ _ -> True
          _ -> False


        chooseParams :: Species -> MutParams
        chooseParams s = if specSize s >= largeSize params then mParams else mParamsS
        {-# INLINE chooseParams #-}
        
        initSpecs = popSpecs pop

        oneEval :: Strategy (Species, TestResult)
        oneEval = evalTuple2 r0 rseq

        -- | Map to fitness data from runFitTest
        fits = M.intersectionWith (,) initSpecs tResults `using` parTraversable oneEval


        -- | Whether a species deserves to live (did it improve recently?)
        eugenics :: SpecId -> (Species, TestResult) ->
                    Maybe (Species, MultiMap Double Genome, Double)
        eugenics sId (sp, TR{..})
          | maybe False (lastImprovement nSpec >=) (dropTime params)
            && sId /= popBSpec pop = Nothing
          | otherwise = Just (nSpec, trScores, trAdj)
          where nSpec = updateSpec trSS sp

        -- | Species that have improved recently enough.
        masterRace :: Map SpecId (Species, MultiMap Double Genome, Double)
        masterRace = M.mapMaybeWithKey eugenics fits

        -- | toList'd version of masterRace
        masterList :: [(SpecId,(Species, MultiMap Double Genome, Double))]
        masterList = M.toList masterRace

        -- | The best
        idVeryBest :: (SpecId, Species)
        idVeryBest = maximumBy (compare `on` (bestScore . specScore . snd)) $ map clean masterList
          where clean (sId,(sp, _, _)) = (sId,sp)

        veryBest = snd idVeryBest

        bestId = fst idVeryBest

        -- | Species to make buckets of
        masterSpec :: Map SpecId Species
        masterSpec = M.map (\(s,_,_) -> s) masterRace

        totalFitness = M.foldl' (+) 0 . M.map (\(_,_,x) -> x) $ masterRace

        totalSize = popSize pop

        dubSize = fromIntegral totalSize

        -- | Distribution of species.
        candSpecs :: MonadRandom m => [(MutParams, Int, m (Double,Genome))]
        candSpecs = zip3 ps realShares pickers
          where sortedMaster = sortBy revComp masterList
                -- | Reversed comparison on best score, to get a descending sorted list
                revComp (_,(sp1,_,_)) (_,(sp2,_,_)) = (compare `on` (bestScore . specScore)) sp2 sp1
                initShares = snd $ mapAccumL share 0 sortedMaster
                share skim (_,(_, _, adj)) = (newSkim, actualShare)
                  where everything = adj / totalFitness * dubSize + skim
                        actualShare = floor everything
                        newSkim = everything - fromIntegral actualShare
                remaining = totalSize - foldl' (+) 0 initShares
                distributeRem _ [] = error "Should run out of numbers first"
                distributeRem n l@(x:xs)
                  | n > 0 = x + 1 : distributeRem (n - 1) xs
                  | n < 0 = error "Remainder should be positive"
                  | otherwise = l
                realShares = distributeRem remaining initShares
                pickers :: MonadRandom m => [m (Double, Genome)]
                pickers = map picker sortedMaster
                  where picker (_,(s, mmap, _)) =
                          let numToTake = specSize s `div` 5 + 1
                              desc = M.toDescList $ MM.toMap mmap
                              toPairs (k, vs) = map (\v -> (k,v)) vs
                              culled = take numToTake $ desc >>= toPairs
                          in uniform culled
                ps = map (\(_,(s,_,_)) -> chooseParams s) sortedMaster

        applyN :: Monad m => Int -> (a -> m a) -> a -> m a
        applyN 0 _  x = return x
        applyN n h !x = h x >>= applyN (n - 1) h

        -- | Generate the genomes for a species
        specGens :: (MonadFresh InnoId m, MonadRandom m) =>
                    Map ConnSig InnoId -> (MutParams, Int, m (Double, Genome)) ->
                    m (Map ConnSig InnoId, [Genome])
        specGens inns (p, n, gen) = applyN n genOne (inns, [])
          where genOne (innos, gs)
                  | isPruning = do
                      (_,parent) <- gen
                      g <- mutateSub p parent
                      return (innos, g:gs)
                  | otherwise =  do
                      roll <- getRandomR (0,1)
                      if roll <= noCrossover p
                        then do
                        (_,parent) <- gen
                        (innos', g) <- mutateAdd p innos parent
                        return (innos', g:gs)
                        else do
                        (fit1, mom) <- gen
                        (fit2, dad) <- gen
                        (innos', g) <- if fit1 > fit2
                                       then breed p innos mom dad
                                       else breed p innos dad mom
                        return (innos', g:gs)
                
        allGens :: (MonadRandom m, MonadFresh InnoId m) => m [Genome]
        allGens = liftM (concat . snd) $ foldM ag' (M.empty, []) candSpecs
          where ag' (innos, cands) cand = do
                  (innos', specGen) <- specGens innos cand
                  return $ (innos', specGen:cands)
                  

        genNewSpecies :: (MonadRandom m, MonadFresh InnoId m) => m (Map SpecId Species, SpecId)
        genNewSpecies = do
          gens <- allGens
          return $ runSpecM (speciate params masterSpec gens) (nextSpec pop)

        generated :: Population
        generated = fst $ runPopM generate (popCont pop)


        generate :: PopM Population
        generate = do
          (specs, nextSpec') <- genNewSpecies
          let specCount = M.size specs
              newParams :: Parameters
              newParams =
                case specParams params of
                 Simple _ -> params
                 Target dp st@SpeciesTarget{..}
                   | specCount > snd targetCount ->
                       let newDP = dp { delta_t = delta_t dp + adjustAmount }
                       in params { specParams = Target newDP st }
                   | specCount < fst targetCount ->
                       let newDP = dp { delta_t = delta_t dp - adjustAmount }
                       in params { specParams = Target newDP st }
                   | otherwise -> params
                             

              bScoreNow = (bestScore . specScore) veryBest
              bOrgNow = (bestGen . specScore) veryBest
              bSpecNow = bestId
              (bScore, bOrg, bSpec) =
                if bScoreNow > popBScore pop
                then (bScoreNow, bOrgNow, bSpecNow)
                else (popBScore pop, popBOrg pop, popBSpec pop)
          cont' <- PopM get
          return pop { popSpecs = specs
                     , popBScore = bScore
                     , popBOrg = bOrg
                     , popBSpec = bSpec
                     , popCont = cont'
                     , popParams = newParams
                     , nextSpec = nextSpec'
                     , popGen = popGen pop + 1
                     , popPhase = newPhase
                     } 


-- | Train the population n times. Values less than 1 return the original.
trainN :: (Applicative f, Monad f) =>
          TrainMethod f -> Int -> Population -> f Population
trainN tm n p
  | n <= 0 = return p
  | otherwise = applyN n (trainOnce tm) (return p)
  where applyN n' h !x = iterate (>>= h) x !! n'


-- | Train without effects
trainPure :: GenScorer a -> Population -> Population
trainPure gs pop = runIdentity $ trainOnce (pureTrain gs) pop


-- | Train until the provided goal is reached, or the max number
-- of generations (first parameter) is reached. Possibly also returns a solution
-- and the number of generations elapsed.
trainUntil :: Int -> GenScorer a -> Population -> (Population, Maybe (Genome, Int))
trainUntil n f p
  | n <= 0 = (p, Nothing)
  | otherwise = go n p
  where go 0  !p' = (p', Nothing)
        go n' !p' = case trainOnce (winTrain f) p' of
                     (First Nothing, p'') -> go (n' - 1) p''
                     (First (Just g), p'') -> (p'', Just (g, n - n'))


-- | Gets the number of species
speciesCount :: Population -> Int
speciesCount Population{..} = M.size popSpecs


-- | Average genome complexity of a population
avgComplexity :: Population -> Double
avgComplexity pop = fromIntegral (totalComplexityMap (popSpecs pop)) / fromIntegral (popSize pop)


-- | Helper for avgComplexity
totalComplexityMap :: Map SpecId Species -> Int
totalComplexityMap smap = M.foldl' (+) 0 . M.map speciesComplexity $ smap


-- | Validate a population, possibly returning a list of errors
validatePopulation :: Population -> Maybe [String]
validatePopulation Population{..} = case errRes of
                                     [] -> Nothing
                                     xs -> Just xs
  where totalSSize = M.foldl' (\acc x -> specSize x + acc) 0 popSpecs
        goodSize
          | totalSSize == popSize = []
          | otherwise = ["Population size differs from actual size"]
        goodSId
          | (not . M.null) popSpecs && fst (M.findMax popSpecs) < nextSpec = []
          | otherwise = ["SpecId lower than extant species"]
        specErrs = concat . M.elems $ M.mapMaybe validateSpecies popSpecs
        errRes = goodSId  ++ goodSize ++ specErrs
