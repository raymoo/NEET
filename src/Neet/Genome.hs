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
Portability : ghc
-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Control.Monad.Random
import Data.Map.Strict (Map)
import qualified Data.Traversable as T
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S

import Control.Monad.Fresh.Class
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


-- | Mutate the weights - perturb or make entirely new weights
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


-- | Signature of a connection, used in matching innovations fromthe same generation.
data ConnSig = ConnSig NodeId NodeId
             deriving (Show, Eq, Ord)


-- | Get a 'ConnSig'
toConnSig :: ConnGene -> ConnSig
toConnSig gene = ConnSig (connIn gene) (connOut gene)

-- | Adds a single connection, updating the innovation context
addConn :: MonadFresh InnoId m => ConnGene ->
           (Map ConnSig InnoId, Map InnoId ConnGene) ->
           m (Map ConnSig InnoId, Map InnoId ConnGene)
addConn conn (innos, conns) = case M.lookup siggy innos of
  Just inno -> return (innos, M.insert inno conn conns)
  Nothing -> do
    newInno <- fresh
    return (M.insert siggy newInno innos, M.insert newInno conn conns)
  where siggy = toConnSig conn


-- | Mutation of additional connection. 'Map' parameter is context of previous
-- innovations. This could be global, or per species generation.
mutateConn :: (MonadFresh InnoId m, MonadRandom m) =>
              Parameters -> Map ConnSig InnoId -> Genome -> m (Map ConnSig InnoId, Genome)
mutateConn params innos g = do
  roll <- getRandomR (0,1)
  if roll > addConnRate params
    then return (innos, g)
    else case allowed of
          [] -> return (innos, g)
          xs -> do
             (innos', conns') <- addRandConn innos (connGenes g)
             return $ (innos', g { connGenes = conns' })
             
  where 
        -- | Which connections are already filled up by genes. Value is a dummy
        -- value because taken is only used in difference anyway.
        taken :: Map ConnSig Bool
        taken = M.fromList . map (\c -> (toConnSig c, True)) . M.elems . connGenes $ g

        -- | Whether a gene is an input gene
        notInput (NodeGene Input _) = False
        notInput _                  = True

        -- | The genome's nodes, in an assoc list
        nodes = M.toList $ nodeGenes g

        -- | Nodes that are not input
        nonInputs = filter (notInput . snd) nodes

        -- | Make a pair of 'ConnSig' and the recurrentness
        makePair (n1,g1) (n2,g2) = (ConnSig n1 n2, yHint g2 <= yHint g1)

        -- | Possible input -> output pairs
        candidates = M.fromList $ makePair <$> nodes <*> nonInputs

        -- | Which pairs are not taken
        allowed = M.toList $ M.difference candidates taken

        -- | Picks one of the available pairs
        pickOne :: MonadRandom m => m (ConnSig, Bool)
        pickOne = uniform allowed

        -- | Randomly chooses one of the available connections and creates a
        -- gene for it
        addRandConn :: (MonadRandom m, MonadFresh InnoId m) =>
                       Map ConnSig InnoId -> Map InnoId ConnGene ->
                       m (Map ConnSig InnoId, Map InnoId ConnGene)
        addRandConn innos conns = do
          (ConnSig inNode outNode, recc) <- pickOne
          w <- getRandomR (-1,1)
          let newConn = ConnGene inNode outNode w True recc
          addConn newConn (innos,conns)


-- | Mutation of additional node.
mutateNode :: (MonadRandom m, MonadFresh InnoId m) =>
              Parameters -> Map ConnSig InnoId ->
              Genome -> m (Genome, Map ConnSig InnoId)
mutateNode params innos g = do
  roll <- getRandomR (0,1)
  if roll <= addNodeRate params then addRandNode else return (g, innos)
  where conns = connGenes g
        nodes = nodeGenes g

        -- | Pick one of the 'InnoId' 'ConnGene' pairs from conns
        pickConn :: MonadRandom m => m (InnoId, ConnGene)
        pickConn = uniform $ M.toList conns

        -- | What will the new node's ID be
        newId = nextNode g

        -- | What should 'nextNode' be updated to
        newNextNode = case newId of NodeId x -> NodeId (x + 1)

        -- | Takes a connection gene and its associated InnoID, and splits
        -- it with a node
        addNode :: MonadFresh InnoId m =>
                   InnoId -> ConnGene -> m (Genome, Map ConnSig InnoId)
        addNode inno gene = do
          let ConnSig inId outId = toConnSig gene

              -- | Gene of the input node of this connection
              inGene = nodes M.! inId

              -- | Gene of the output node of this connection
              outGene = nodes M.! outId

              -- | The new node gene
              newGene = NodeGene Hidden ((yHint inGene + yHint outGene) / 2)

              -- | The new map of nodes, after inserting the new one
              newNodes = M.insert newId newGene nodes

              -- | The disabled version of the old connection
              disabledConn = gene { connEnabled = False }

              -- | The gene for the connection between the input and the new node
              backGene = ConnGene inId newId (connWeight gene) True (connRec gene)

              -- | The gene for the connection between the new node and the output
              forwardGene = ConnGene newId outId 1 True (connRec gene)
              
          (innos', newConns) <-
            addConn disabledConn >=> addConn backGene >=> addConn forwardGene $ (innos, conns)

          return $ (g { nodeGenes = newNodes
                      , connGenes = newConns
                      , nextNode = newNextNode
                      }, innos')

        -- | Pick an available connection randomly and make a gene for it
        addRandNode :: (MonadRandom m, MonadFresh InnoId m) => m (Genome, Map ConnSig InnoId)
        addRandNode =
          pickConn >>= uncurry addNode
