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
Module      : Neet.Genome
Description : Encodings NEAT genomes
Copyright   : (c) Leon Medvinsky, 2015

License     : GPL-3
Maintainer  : lmedvinsky@hotmail.com
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE RecordWildCards #-}

module Neet.Genome ( -- * Genes
                     NodeId(..)
                   , NodeType(..)
                   , NodeGene(..)
                   , ConnGene(..)
                   , InnoId(..)
                     -- * Genome
                   , Genome(..)
                     -- ** Construction
                   , fullConn
                   ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Random.Class
import Data.Map.Strict (Map)
import qualified Data.Traversable as T
import qualified Data.Map.Strict as M

import Neet.Parameters

-- | The IDs node genes use to refer to nodes.
newtype NodeId = NodeId Int
               deriving (Show, Eq, Ord)


-- | Types of nodes
data NodeType = Input | Hidden | Output
              deriving (Show)


-- | Node genes
data NodeGene = NodeGene { nodeType :: NodeType
                         , yHint :: Rational -- ^ A hint for recurrency
                         }
              deriving (Show)


-- | Connection genes
data ConnGene = ConnGene { connIn :: NodeId
                         , connOut :: NodeId
                         , connWeight :: Double
                         , connEnabled :: Bool
                         , connRec :: Bool -- ^ A hint for recurrency
                         }
              deriving (Show)


-- | Innovation IDs
newtype InnoId = InnoId Int
               deriving (Show, Eq, Ord)


-- | A NEAT genome. The innovation numbers are stored in here, and not the genes,
-- to prevent data duplication.
data Genome =
  Genome { nodeGenes :: Map NodeId NodeGene
         , connGenes :: Map InnoId ConnGene
         , nextNode :: NodeId
         }
  deriving (Show)


-- | Takes the number of inputs, the number of outputs, and gives a genome with
-- the inputs fully connected to the outputs with random weights. The order of
-- the connections are deterministic, so when generating a population, you
-- can just start the innovation number at iSize * oSize + 1
fullConn :: MonadRandom m => Int -> Int -> m Genome
fullConn iSize oSize = do
  let inIDs = map NodeId [1..iSize]
      outIDs = map NodeId [iSize + 1..oSize + iSize]
      inputGenes = zip inIDs $ repeat (NodeGene Input 0)
      outputGenes = zip outIDs $ repeat (NodeGene Output 1)
      nodeGenes = M.fromList $ inputGenes ++ outputGenes
      nextNode = NodeId $ iSize + oSize + 1
      nodePairs = (,) <$> inIDs <*> outIDs
  conns <- zipWith (\(inN, outN) w -> ConnGene inN outN w True False) nodePairs `liftM` getRandomRs (-1,1)
  let connGenes = M.fromList $ zip (map InnoId [1..iSize * oSize]) conns
  return $ Genome{..}


mutateWeights :: MonadRandom m => Parameters -> Genome -> m Genome
mutateWeights Parameters{..} g@Genome{..} = setConns g `liftM` T.mapM mutOne connGenes
  where setConns g cs = g { connGenes = cs }
        mutOne conn = do
          roll <- getRandomR (0,1)
          roll2 <- getRandomR (0,1)
          let newWeight
                | roll > mutWeightRate = return $ connWeight conn
                | roll2 <= newWeightRate = getRandomR (-1,1)
                | otherwise = do
                    pert <- getRandomR (-pertAmount,pertAmount)
                    return $ connWeight conn + pert
          w <- newWeight
          return $ conn { connWeight = w }
