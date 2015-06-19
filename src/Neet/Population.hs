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
module Neet.Population (
                         Population(..)
                         -- * PopM
                       , PopM
                       , PopContext
                       , runPopM
                         -- * Construction
                       , PopSettings(..)
                       , newPop
                         -- * Training
                       , trainOnce
                       ) where

import Neet.Species
import Neet.Genome
import Neet.Parameters

import Data.MultiMap (MultiMap)
import qualified Data.MultiMap as MM

import Data.Map (Map)
import qualified Data.Map as M

import Data.List (foldl', maximumBy)

import Data.Maybe

import Control.Monad.Random
import Control.Monad.Fresh.Class
import Control.Monad.Trans.State

import Control.Applicative
import Control.Monad

import Control.Arrow (first, second)

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
             , popParams  :: Parameters        -- ^ Parameters for large species
             , popParamsS :: Parameters        -- ^ Parameters for small species
             }
  deriving (Show)


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
     , psParamsS :: Parameters -- ^ Parameters for small species
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
shuttleOrgs p@Parameters{..} buckets gs = foldM shutOne buckets gs
  where DistParams{..} = distParams
        shutOne :: MonadFresh SpecId m => [SpecBucket] -> Genome -> m [SpecBucket]
        shutOne (SB sId rep gs:bs) g
          | distance p g rep <= delta_t = return $ SB sId rep (g:gs) : bs
          | otherwise = liftM (SB sId rep gs :) $ shutOne bs g
        shutOne [] g = do
          newId <- fresh
          return $ [SB newId g [g]]


zipWithDefaults :: (a -> b -> Maybe c) -> (a -> Maybe c) -> (b -> Maybe c) -> [a] -> [b] -> [c]
zipWithDefaults f da db [] bs = mapMaybe db bs
zipWithDefaults f da db as [] = mapMaybe da as
zipWithDefaults f da db (a:as) (b:bs) =
  case f a b of
   Just res -> res : zipWithDefaults f da db as bs
   Nothing -> zipWithDefaults f da db as bs


-- | Speciation function
speciate :: MonadFresh SpecId m =>
            Parameters -> Map SpecId Species -> [Genome] -> m (Map SpecId Species)
speciate params specs gs = do
  filled <- fill
  let zipped = zipWithDefaults mkSpecies (const Nothing) newSpecies specL filled
  return $ M.fromList zipped
  where oneBucket (k, Species _ (rep:_) _ _) = SB k rep []
        assocs = M.toList specs
        specL = map snd assocs
        buckets = map oneBucket assocs
        fill = shuttleOrgs params buckets gs
        mkSpecies (Species _ _ scr imp) (SB sId _ gs)
          | null gs = Nothing
          | otherwise = Just $ (sId, Species (length gs) gs scr imp)
        newSpecies (SB _ _ []) = Nothing
        newSpecies (SB sId _ (g:gs)) = Just $ (sId, newSpec g gs)


-- | Generates a fully connected starter population, given a seed.
newPop :: Int -> PopSettings -> Population
newPop seed PS{..} = fst $ runPopM generate initCont
  where popSize = psSize
        popBScore = 0
        popBSpec = SpecId 1
        initCont = PC (InnoId $ psInputs * psOutputs + 2) (mkStdGen seed)
        popParams = psParams
        popParamsS = psParamsS
        generateGens = replicateM psSize (fullConn psInputs psOutputs)
        generate = do
          gens <- generateGens
          let (popSpecs, nextSpec) = runSpecM (speciate psParams M.empty gens) (SpecId 1)
              popBOrg = head gens
          popCont <- PopM get
          return Population{..}


data BS = Big | Small


-- | Advances the population one generation with the fitness function.
trainOnce :: (Genome -> Double) -> Population -> Population
trainOnce f pop = generated
  where params = popParams pop
        paramsS = popParamsS pop

        chooseParams :: Species -> Parameters
        chooseParams s = if specSize s >= largeSize params then params else paramsS
        {-# INLINE chooseParams #-}
        
        initSpecs = popSpecs pop

        -- | Map to fitness data from runFitTest
        fits = M.map (\sp -> (sp, runFitTest f sp)) initSpecs

        -- | Whether a species deserves to live (did it improve recently?)
        eugenics :: SpecId -> (Species, (MultiMap Double Genome, SpecScore, Double)) ->
                    Maybe (Species, MultiMap Double Genome, Double)
        eugenics sId (sp, (fitmap, ss, adj))
          | maybe False (lastImprovement nSpec >=) (dropTime params)
            && sId /= popBSpec pop = Nothing
          | otherwise = Just (nSpec, fitmap, adj)
          where nSpec = updateSpec ss sp

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
        candSpecs :: MonadRandom m => [(Parameters, Int, m Genome)]
        candSpecs = zip3 ps realShares pickers
          where initShares = map share masterList
                share (_,(_, _, adj)) = round $ adj / totalFitness * dubSize
                remaining = totalSize - foldl' (+) 0 initShares
                distributeRem n [] = error "Should run out of numbers first"
                distributeRem n l@(x:xs)
                  | n > 0 = x + 1 : distributeRem (n - 1) xs
                  | otherwise = l
                realShares = distributeRem remaining initShares
                pickers :: MonadRandom m => [m Genome]
                pickers = map picker masterList
                  where picker (_,(_, mmap, _)) =
                          fromList . map (\(d,g) -> (g, toRational d)) $ MM.toList mmap
                ps = map (\(_,(s,_,_)) -> chooseParams s) masterList

        applyN :: Monad m => Int -> (a -> m a) -> a -> m a
        applyN 0 f x = return x
        applyN n f !x = f x >>= applyN (n - 1) f

        -- | Generate the genomes for a species
        specGens :: (MonadFresh InnoId m, MonadRandom m) =>
                    (Parameters, Int, m Genome) -> m [Genome]
        specGens (p, n, gen) = liftM snd $ applyN n genOne (M.empty, [])
          where genOne (innos, gs) = do
                  roll <- getRandomR (0,1)
                  if roll <= noCrossover p
                    then do
                    parent <- gen
                    (innos', g) <- mutate p innos parent
                    return (innos', g:gs)
                    else do
                    mom <- gen
                    dad <- gen
                    g <- crossover p mom dad
                    return (innos, g:gs)

        allGens :: (MonadRandom m, MonadFresh InnoId m) => m [Genome]
        allGens = liftM concat $ mapM specGens candSpecs

        genNewSpecies :: (MonadRandom m, MonadFresh InnoId m) => m (Map SpecId Species, SpecId)
        genNewSpecies = do
          gens <- allGens
          return $ runSpecM (speciate params masterSpec gens) (nextSpec pop)

        generated :: Population
        generated = fst $ runPopM generate (popCont pop)

        generate :: PopM Population
        generate = do
          (specs, nextSpec') <- genNewSpecies
          let bScoreNow = (bestScore . specScore) veryBest
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
                     , nextSpec = nextSpec'
                     } 


-- | Train the population n times. Values less than 1 return the original.
trainN :: Int -> (Genome -> Double) -> Population -> Population
trainN n f p
  | n <= 0 = p
  | otherwise = applyN n (trainOnce f) p
  where applyN 0 f !x = x
        applyN n f !x = applyN (n - 1) f (f x)
