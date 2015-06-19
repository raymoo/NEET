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
{-# LANGUAGE MultiParamTypeClasses #-}
module Neet.Population (
                       ) where

import Neet.Species
import Neet.Genome

import Data.MultiMap (MultiMap)
import qualified Data.MultiMap as MM

import Data.Map (Map)
import qualified Data.Map as M

import Control.Monad.Random
import Control.Monad.Fresh.Class

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
             , nextInno  :: InnoId             -- ^ The next innovation
             , randGen   :: StdGen
             }


newtype PopM a = PopM (Population -> (Population, a))
            deriving (Functor)


instance Applicative PopM where
  pure a = PopM $ \s -> (s, a)
  PopM f <*> PopM g = PopM $ \s ->
    let (s', f') = f s
        (s'', a) = g s'
    in (s'', f' a)


instance Monad PopM where
  return = pure
  PopM f >>= k = PopM $ \s ->
    let (s', a) = f s
        PopM g = k a
    in g s'


instance MonadRandom PopM where
  getRandom = PopM $ \s ->
    let (r, gen) = random (randGen s)
    in (s { randGen = gen }, r)
  getRandoms = PopM $ \s ->
    let (g1, g2) = split $ randGen s
    in (s { randGen = g2 }, randoms g1)
  getRandomR range = PopM $ \s ->
    let (r, gen) = randomR range (randGen s)
    in (s { randGen = gen }, r)
  getRandomRs range = PopM $ \s ->
    let (g1, g2) = split $ randGen s
    in (s { randGen = g2 }, randomRs range g1)


instance MonadFresh InnoId PopM where
  fresh = PopM $ \s ->
    let inno@(InnoId x) = nextInno s
    in (s { nextInno = InnoId $ x + 1 }, inno)
