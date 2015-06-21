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

module Neet.Examples.XOR (xorFit, andFit, orFit) where


import Neet


boolQuestions :: [[Double]]
boolQuestions = [ [0, 0]
                , [0, 1]
                , [1, 0]
                , [1, 1]
                ]

xorAnswers :: [Double]
xorAnswers = [0, 1, 1, 0]

sampleFit :: [[Double]] -> [Double] -> Genome -> Double
sampleFit questions answers g = max 0 $ (fromIntegral (length answers) - sumDiffs)**2
  where net = mkPhenotype g
        try samp = head . getOutput $ snapshot net samp
        responses = map try questions
        sumDiffs = sum $ zipWith (\x y -> abs (x - y)) responses answers

xorFit :: Genome -> Double
xorFit = sampleFit boolQuestions xorAnswers

andAnswers :: [Double]
andAnswers = [0, 0, 0, 1]


andFit :: Genome -> Double
andFit = sampleFit boolQuestions andAnswers


orAnswers :: [Double]
orAnswers = [0, 1, 1, 1]


orFit :: Genome -> Double
orFit = sampleFit boolQuestions orAnswers
