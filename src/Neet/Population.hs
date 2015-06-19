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
module Neet.Population (
                         Population(..)
                         -- * PopM
                       , PopM
                       , PopContext
                       , runPopM
                         -- * Construction
                       , PopSettings(..)
                       ) where

import Neet.Species
import Neet.Genome
import Neet.Parameters

import Data.MultiMap (MultiMap)
import qualified Data.MultiMap as MM

import Data.Map (Map)
import qualified Data.Map as M

import Data.List (foldl')

import Data.Maybe

import Control.Monad.Random
import Control.Monad.Fresh.Class
import Control.Monad.Trans.State

import Control.Applicative
import Control.Monad

newtype SpecId = SpecId Int
               deriving (Show, Eq, Ord)


-- | A NEAT Population
data Population =
  Population { popSize   :: Int                -- ^ Size of the population
             , popSpecs  :: Map SpecId Species -- ^ The species
             , popBScore :: Double             -- ^ Best score so far
             , popBOrg   :: Genome             -- ^ Best genome so far
             , popBSpec  :: SpecId             -- ^ Id of the species that hosted the best score
             , popCont   :: PopContext         -- ^ Tracking state and fresh values
             , nextSpec  :: SpecId             -- ^ The next species ID
             , popParams  :: Parameters        -- ^ Parameters for large species
             , popParamsS :: Parameters        -- ^ Parameters for small species
             }


data PopContext =
  PC { nextInno :: InnoId
     , randGen  :: StdGen
     } 


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
