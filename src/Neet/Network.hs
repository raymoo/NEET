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
Module      : Neet.Network
Description : Networks produced from NEAT Genomes
Copyright   : (c) Leon Medvinsky, 2015

License     : GPL-3
Maintainer  : lmedvinsky@hotmail.com
Stability   : experimental
Portability : ghc
-}

module Neet.Network (
                      -- * Sigmoid
                      modSig
                      -- * Network
                    , Network(..)
                      -- ** Neuron
                    , Neuron(..)
                      -- *** Updates
                    , stepNeuron
                    ) where

import Data.Map (Map)
import qualified Data.Map as M

import Neet.Genome


-- | Modified sigmoid function from the original NEAT paper
modSig :: Double -> Double
modSig d = 1 / (1 + exp (-4.9 * d))


-- | A single neuron
data Neuron =
  Neuron { activation  :: Double            -- ^ The current activation
         , connections :: Map NodeId Double -- ^ The inputs to this Neuron
         }
  deriving (Show)
           

-- | Sparse recurrent network, like those made by NEAT
data Network =
  Network { netInputs   :: [NodeId] -- ^ Which nodes are inputs
          , netOutputs  :: [NodeId] -- ^ Which nodes are outputs
          , netState    :: Map NodeId Neuron
          } 
  deriving (Show)


-- | Takes the previous step's activations and gives a function to update a neuron.
stepNeuron :: Map NodeId Double -> Neuron -> Neuron
stepNeuron acts (Neuron _ conns) = Neuron (modSig weightedSum) conns
  where oneFactor nId w = (acts M.! nId) * w
        weightedSum = M.foldlWithKey' (\acc k w -> acc + oneFactor k w) 0 conns
