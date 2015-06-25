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
Module      : Neet.Parameters
Description : Learning parameters
Copyright   : (c) Leon Medvinsky, 2015

License     : GPL-3
Maintainer  : lmedvinsky@hotmail.com
Stability   : experimental
Portability : portable
-}

module Neet.Parameters ( Parameters(..)
                       , defParams
                       , DistParams(..)
                       , defDistParams
                       , MutParams(..)
                       , defMutParams
                       , defMutParamsS
                       , SpeciesParams(..)
                       , distParams
                       , SpeciesTarget(..)
                       , SearchStrat(..)
                       , PhaseParams(..)
                       , PhaseState(..)
                       ) where


-- | The genetic parameters
data Parameters =
  Parameters { mutParams  :: MutParams
             , mutParamsS :: MutParams     -- ^ Mutation parameters for small populations
             , largeSize  :: Int           -- ^ The minimum size for a species to be considered large
             , specParams :: SpeciesParams -- ^ Parameters for the distance function
             , dropTime   :: Maybe Int     -- ^ Drop a species if it doesn't improve for this long,
                                           -- and it hasn't hosted the most successful genome.
             }
  deriving (Show)


-- | Settings for distance. `Simple` is for fixed distance calculations. `Target`
-- should be used when you want the threshold value for distance to change to
-- try to meet a desired species count.
data SpeciesParams = Simple DistParams
                   | Target DistParams SpeciesTarget 
                   deriving (Show)


distParams :: SpeciesParams -> DistParams
distParams (Simple dp) = dp
distParams (Target dp _) = dp


-- | Distance Parameters
data DistParams =
  DistParams { dp1 :: Double -- ^ Coefficient to the number of excess genes
             , dp2 :: Double -- ^ Coefficient to the number of disjoint genes
             , dp3 :: Double -- ^ Coefficient to the average weight differences
             , delta_t :: Double -- ^ How close counts as the same species
             } 
  deriving (Show)


-- | How to seek a target species count
data SpeciesTarget =
  SpeciesTarget { targetCount  :: (Int,Int) -- ^ Desired range of species count, inclusive
                , adjustAmount :: Double    -- ^ How much to adjust the distance threshold
                                            -- if there are too many/not enough species
                } 
  deriving (Show)


-- | Mutation Parameters
data MutParams =
  MutParams { mutWeightRate  :: Double -- ^ How often weights are mutated
            , newWeightRate  :: Double -- ^ How often weights are replaced if mutated
            , pertAmount     :: Double -- ^ Max amount of perturbation
            , weightRange    :: Double -- ^ A new max is between negative this and positive this
            , addConnRate    :: Double -- ^ How often new connections are made
            , addNodeRate    :: Double -- ^ How often new nodes are added
            , delConnChance  :: Double -- ^ How likely it is for a connection to
                                       -- be erased
            , delNodeChance  :: Double -- ^ How likely it is for a node to be erased
            , recurrencies   :: Bool   -- ^ Whether to allow recurrent connections
            , noCrossover    :: Double -- ^ Percent of population that mutates without crossover
            , disableChance  :: Double -- ^ How likely that a disabled parent results
                                       -- in a disabled child
            }
  deriving (Show)


-- | Mutation parameters for defParams
defMutParams :: MutParams
defMutParams =
  MutParams { mutWeightRate = 0.8
            , newWeightRate = 0.1
            , pertAmount = 2.5
            , weightRange = 2.5
            , addConnRate = 0.3
            , addNodeRate = 0.03
            , delConnChance = 0
            , delNodeChance = 0
            , recurrencies = False
            , noCrossover = 0.25
            , disableChance = 0.75
            } 


-- | The parameters used in the original NEAT paper, except the perturbation amount
-- and threshold for size.
defParams :: Parameters
defParams =
  Parameters { mutParams = defMutParams
             , mutParamsS = defMutParamsS
             , largeSize = 20
             , specParams = Simple defDistParams
             , dropTime = Just 15
             } 


defMutParamsS :: MutParams
defMutParamsS = defMutParams { addConnRate = 0.05 }


-- | Parameters used for distance in the paper
defDistParams :: DistParams
defDistParams = DistParams 1 1 0.4 3


-- | Search Strategy
data SearchStrat = Complexify
                 | Phased PhaseParams
                 deriving (Show)


-- | Parameters for phased search
data PhaseParams =
  PhaseParams { phaseAddAmount :: Double -- ^ How much to add to the mean complexity
                                         -- to get the next complexity
              , phaseWaitTime  :: Int    -- ^ How many generations without a drop
                                         -- in complexity warrants going back to
                                         -- a complexify strategy
              } 
  deriving (Show)


-- | State of phasing
data PhaseState = Complexifying Double -- ^ The argument is the current threshold
                                       -- to start pruning at.
                | Pruning Int Double   -- ^ The first argument is how many generations
                                       -- the mean complexity has not fallen. The second
                                       -- is the last mean complexity.
                deriving (Show)
