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
Module      : Neet.Examples.XOR
Description : Testing the algorithm on XOR
Copyright   : (c) Leon Medvinsky, 2015

License     : GPL-3
Maintainer  : lmedvinsky@hotmail.com
Stability   : experimental
Portability : ghc
-}

module Neet.Examples.XOR (xorFit, andFit, orFit, xorExperiment) where


import Neet
import Neet.Species

import qualified Data.Map.Strict as M

import System.Random

import Data.List (intercalate)


boolQuestions :: [[Double]]
boolQuestions = [ [0, 0]
                , [0, 1]
                , [1, 0]
                , [1, 1]
                ]

xorAnswers :: [Bool]
xorAnswers = [False, True, True, False]

sampleFit :: [[Double]] -> [Bool] -> GenScorer [Double]
sampleFit questions answers = GS intermed ff criteria
  where intermed g = map try questions
          where try samp = head $ pushThrough net samp
                net = mkPhenotype g
        ff ds = (fromIntegral (length answers) - sumDiffs)**2
          where sumDiffs = sum $ zipWith (\x y -> abs (x - y)) ds binarized
        binarized = map (\b -> if b then 1 else 0) answers
        bounds = map (\b -> if b then (>0.5) else (<0.5)) answers
        criteria ds = and $ zipWith id bounds ds

xorFit :: GenScorer [Double]
xorFit = sampleFit boolQuestions xorAnswers

andAnswers :: [Bool]
andAnswers = [False, False, False, True]


andFit :: GenScorer [Double]
andFit = sampleFit boolQuestions andAnswers


orAnswers :: [Bool]
orAnswers = [False, True, True, True]


orFit :: GenScorer [Double]
orFit = sampleFit boolQuestions orAnswers


-- | Automated XOR experiment
xorExperiment :: IO ()
xorExperiment = do
  putStrLn $ "XOR Input list is: " ++ show boolQuestions
  putStrLn "Press Enter to start learning"
  _ <- getLine
  putStrLn "Running XOR experiment with 150 population and default parameters"
  seed <- randomIO
  let pop = newPop seed (PS 150 2 1 defParams { specParams = sp } Nothing Nothing)
      sp = Target dp (SpeciesTarget (14,17) 0.1)
      dp = defDistParams { delta_t = 5 }
  (pop', sol) <- xorLoop pop
  printInfo pop'
  putStrLn $ "Solution found in generation " ++ show (popGen pop')
  let score = gScorer xorFit sol
  putStrLn $ "\nOutputs to XOR inputs are: " ++ show score
  putStrLn $ "Fitness (Out of 16): " ++ show (fitnessFunction xorFit score)

  putStrLn $ "Final distance threshold: " ++ show (distParams . specParams $ popParams pop')
  
  putStrLn "\nPress Enter to view network"
  _ <- getLine
  renderGenome sol


mkSpecInfo :: Population -> String
mkSpecInfo pop = intercalate ", " infos
  where infos = map (\((SpecId k), sp) -> "S" ++ show k ++ " P" ++ show (specSize sp)) ass
        ass = M.toList $ popSpecs pop


xorLoop :: Population -> IO (Population, Genome)
xorLoop pop = do
  printInfo pop
  let (pop', mg) = trainOnce xorFit pop
  case mg of
   Nothing -> xorLoop pop'
   Just g -> return (pop',g)
     

printInfo :: Population -> IO ()
printInfo pop = do
  putStrLn $ "Generation " ++ show (popGen pop)
  putStrLn $ "Species: " ++ mkSpecInfo pop
  putStrLn $ "High Score: " ++ show (popBScore pop)
  putStrLn ""
