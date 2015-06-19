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
Module      : Neet.Species
Description : NEAT Species
Copyright   : (c) Leon Medvinsky, 2015

License     : GPL-3
Maintainer  : lmedvinsky@hotmail.com
Stability   : experimental
Portability : ghc
-}

{-# LANGUAGE RecordWildCards #-}
module Neet.Species (
                      Species(..)
                    , SpecScore(..)
                      -- * Construction
                    , newSpec
                      -- * Update/Fitness
                    , runFitTest
                    , updateSpec
                    ) where


import Neet.Genome
import Data.MultiMap (MultiMap)
import qualified Data.MultiMap as MM
import Data.List (foldl')

-- | A NEAT Species.
data Species =
  Species { specSize :: Int
          , specOrgs :: [Genome]    -- ^ All the organisms in this species
          , specScore :: SpecScore
          , lastImprovement :: Int  -- ^ Number of gens ago the best score improved
          }


-- | Scoring data
data SpecScore = SpecScore { bestScore :: Double, bestGen :: Genome }


instance Show Species where
  show (Species siz _ (SpecScore scr _) lastImprov) =
    "Species {specSize = " ++ show siz ++
    ", specOrgs = <...>, bestScore = " ++ show scr ++
    ", bestGen = <...>" ++
    ", lastImprovement = " ++ show lastImprov ++ "}"


-- | Creates a new 'Species' with starter stats from a 'Genome'
newSpec :: Genome -> Species
newSpec gen = Species 0 singleton (SpecScore 0 gen) 0
  where singleton = [gen]


-- | Output the result of testing fitness. Last value is the total adjusted fitness
runFitTest :: (Genome -> Double) -> Species -> (MultiMap Double Genome, SpecScore, Double)
runFitTest f Species{..} = (mmap, ss, adjF)
  where dubSize = fromIntegral specSize :: Double
        (mmap, adjF) = foldl' accumOne (MM.empty, 0) $ map calcOne specOrgs
        calcOne g = let fitness = f g in (fitness, g, fitness / dubSize)
        accumOne (accM, accA) (fit, g, adj) = (MM.insert fit g accM, accA + adj)
        ss = case MM.findMaxWithValues mmap of
              Nothing -> error "(runFitTest) folding fitness resulted in empty map!"
              Just (scr, (x:_)) -> SpecScore scr x


-- | Takes a new SpecScore, new organisms, and updates a species
updateSpec :: SpecScore -> [Genome] -> Species -> Species
updateSpec ss gs spec = spec { specSize = length gs
                             , specOrgs = gs
                             , specScore = newScr
                             , lastImprovement = li
                             }
  where oldScr = specScore spec
        (newScr, li)
          | bestScore ss > bestScore oldScr = (ss, 0)
          | otherwise                       = (oldScr, lastImprovement spec + 1)
