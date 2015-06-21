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
                    , TestResult(..)
                    , runFitTest
                    , updateSpec
                      -- * Statistics
                    , maxDist
                      -- * Debugging
                    , validateSpecies
                    ) where


import Neet.Genome
import Neet.Parameters

import Data.MultiMap (MultiMap)
import qualified Data.MultiMap as MM
import Data.List (foldl')
import Data.Maybe

import Control.Applicative ((<$>), (<*>))


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


-- | Creates a new 'Species' with starter stats from a 'Genome' and the rest
newSpec :: Genome -> [Genome] -> Species
newSpec gen gens = Species (length gens + 1) (gen:gens) (SpecScore 0 gen) 0


-- | A result of evaluating a species
data TestResult =
  TR { trScores :: MultiMap Double Genome -- ^ The score of each organism
     , trSS     :: SpecScore              -- ^ Result 'SpecScore'
     , trAdj    :: Double                 -- ^ Total adjusted fitness
     , trSol    :: Maybe Genome           -- ^ Possible Solution
     }

findMay :: (a -> Bool) -> [a] -> Maybe a
findMay _ [] = Nothing
findMay p (a:as)
  | p a = Just a
  | otherwise = findMay p as


-- | Output the result of testing fitness. Last value is the total adjusted fitness
runFitTest :: GenScorer a -> Species -> TestResult
runFitTest GS{..} Species{..} = TR mmap ss (totF / dubSize) msolution
  where dubSize = fromIntegral specSize :: Double
        (mmap, totF) = foldl' accumOne (MM.empty, 0) resses
        calcOne g = let score = gScorer g in (score, g)
        resses = map calcOne specOrgs
        msolution = fmap snd . findMay (\pair -> winCriteria (fst pair)) $ resses
        accumOne (accM, accA) (score, g) = (MM.insert fit g accM, accA + fit)
          where fit = fitnessFunction score
        ss = case MM.findMaxWithValues mmap of
              Nothing -> error "(runFitTest) folding fitness resulted in empty map!"
              Just (scr, (x:_)) -> SpecScore scr x
              _       -> error "(runFitTest) MultiMap had a key with empty list!"


-- | Takes a new SpecScore and updates the metadata of a species
updateSpec :: SpecScore -> Species -> Species
updateSpec ss spec = spec { specScore = newScr
                          , lastImprovement = li
                          }
  where oldScr = specScore spec
        (newScr, li)
          | bestScore ss > bestScore oldScr = (ss, 0)
          | otherwise                       = (oldScr, lastImprovement spec + 1)


-- | Validates a species, possibly returning errors
validateSpecies :: Species -> Maybe [String]
validateSpecies Species{..} = case orgErrs ++ goodSize of
                               [] -> Nothing
                               xs -> Just xs
  where orgErrs = concat $ mapMaybe validateGenome specOrgs
        goodSize
          | specSize == length specOrgs = []
          | otherwise = ["Species size differs from number of organisms"]
        

-- | Gets the max distance between two genomes in a species
maxDist :: Parameters -> Species -> Double
maxDist ps Species{..} = maximum . map (uncurry (distance ps)) $ (,) <$> specOrgs <*> specOrgs
