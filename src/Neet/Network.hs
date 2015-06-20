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

{-# LANGUAGE RecordWildCards #-}
module Neet.Network (
                      -- * Sigmoid
                      modSig
                      -- * Network
                    , Network(..)
                      -- ** Neuron
                    , Neuron(..)
                      -- ** Construction
                    , mkPhenotype
                      -- ** Updates
                    , stepNeuron
                    , stepNetwork
                    , snapshot
                      -- ** Output
                    , getOutput
                    ) where

import Data.Set (Set)
import qualified Data.Set as S

import Data.List (foldl')

import qualified Data.IntMap as IM
import Data.IntMap (IntMap)

import Neet.Genome


-- | Modified sigmoid function from the original NEAT paper
modSig :: Double -> Double
modSig d = 1 / (1 + exp (-4.9 * d))


-- | A single neuron
data Neuron =
  Neuron { activation  :: Double            -- ^ The current activation
         , connections :: IntMap Double -- ^ The inputs to this Neuron
         , yHint       :: Rational          -- ^ Visualization height
         }
  deriving (Show)
           

-- | Sparse recurrent network, like those made by NEAT
data Network =
  Network { netInputs   :: [NodeId] -- ^ Which nodes are inputs
          , netOutputs  :: [NodeId] -- ^ Which nodes are outputs
          , netState    :: IntMap Neuron
          , netDepth    :: Int      -- ^ Upper bound on depth
          } 
  deriving (Show)


-- | Takes the previous step's activations and current inputs and gives a
-- function to update a neuron.
stepNeuron :: IntMap Double -> Neuron -> Neuron
stepNeuron acts (Neuron _ conns yh) = Neuron (modSig weightedSum) conns yh
  where oneFactor nId w = (acts IM.! nId) * w
        weightedSum = IM.foldlWithKey' (\acc k w -> acc + oneFactor k w) 0 conns


-- | Steps a network one step. Takes the network and the current input, minus
-- the bias.
stepNetwork :: Network -> [Double] -> Network
stepNetwork net@Network{..} ins = net { netState = newNeurons }
  where pairs = zipWith (\x y -> (getNodeId x, y)) netInputs (ins ++ [1])

        acts = IM.map activation netState

        -- | The previous state, except updated to have new inputs
        modState = foldl' (flip $ uncurry IM.insert) acts pairs

        newNeurons = IM.map (stepNeuron modState) netState


-- | Steps a network for at least its depth
snapshot :: Network -> [Double] -> Network
snapshot net = go (netDepth net - 1)
  where go 0 _  = net
        go n ds = stepNetwork (go (n - 1) ds) ds


mkPhenotype :: Genome -> Network
mkPhenotype Genome{..} = (IM.foldl' addConn nodeHusk connGenes) { netInputs = map NodeId ins
                                                                , netOutputs = map NodeId outs
                                                                , netDepth = dep }
  where addNode n@(Network _ _ s _) nId (NodeGene _ yh) =
          n { netState = IM.insert nId (Neuron 0 IM.empty yh) s
            }

        ins = IM.keys . IM.filter (\ng -> nodeType ng == Input) $ nodeGenes
        outs = IM.keys . IM.filter (\ng -> nodeType ng == Output) $ nodeGenes

        -- | Network without connections added
        nodeHusk = IM.foldlWithKey' addNode (Network [] [] IM.empty 0) nodeGenes

        depthSet :: Set Rational
        depthSet = IM.foldl' (flip S.insert) S.empty $ IM.map Neet.Genome.yHint nodeGenes

        dep = S.size depthSet

        addConn2Node nId w (Neuron a cs yh) = Neuron a (IM.insert nId w cs) yh

        addConn net@Network{ netState = s } ConnGene{..}
          | not connEnabled = net
          | otherwise =
              let newS = IM.adjust (addConn2Node (getNodeId connIn) connWeight) (getNodeId connOut) s
              in net { netState = newS }


-- | Gets the output of the current state
getOutput :: Network -> [Double]
getOutput Network{..} = map (activation . (netState IM.!) . getNodeId) netOutputs
