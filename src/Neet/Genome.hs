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
-- can just start the innovation number at (iSize + 1) * oSize, since the network
-- includes an additional input for the bias.
fullConn :: MonadRandom m => Int -> Int -> m Genome
fullConn iSize oSize = do
  let inCount = iSize + 1
      inIDs = map NodeId [1..inCount]
      outIDs = map NodeId [inCount + 1..oSize + inCount]
      inputGenes = zip inIDs $ repeat (NodeGene Input 0)
      outputGenes = zip outIDs $ repeat (NodeGene Output 1)
      nodeGenes = M.fromList $ inputGenes ++ outputGenes
      nextNode = NodeId $ inCount + oSize + 1
      nodePairs = (,) <$> inIDs <*> outIDs
  conns <- zipWith (\(inN, outN) w -> ConnGene inN outN w True False) nodePairs `liftM` getRandomRs (-1,1)
  let connGenes = M.fromList $ zip (map InnoId [1..]) conns
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
              Genome -> m (Map ConnSig InnoId, Genome)
mutateNode params innos g = do
  roll <- getRandomR (0,1)
  if roll <= addNodeRate params then addRandNode else return (innos, g)
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
                   InnoId -> ConnGene -> m (Map ConnSig InnoId, Genome)
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

          return $ (innos', g { nodeGenes = newNodes
                              , connGenes = newConns
                              , nextNode = newNextNode
                              })

        -- | Pick an available connection randomly and make a gene for it
        addRandNode :: (MonadRandom m, MonadFresh InnoId m) => m (Map ConnSig InnoId, Genome)
        addRandNode =
          pickConn >>= uncurry addNode


-- | Mutates the genome, using the specified parameters and innovation context.
mutate :: (MonadRandom m, MonadFresh InnoId m) => Parameters -> Map ConnSig InnoId ->
          Genome -> m (Map ConnSig InnoId, Genome)
mutate params innos g = do
  g' <- mutateWeights params g
  uncurry (mutateNode params) >=> uncurry (mutateConn params) $ (innos, g)


-- | Super left biased merge -- loners on the right map don't get in
superLeft :: Ord k => (a -> b -> c) -> (a -> c) -> Map k a -> Map k b -> Map k c
superLeft comb mk = M.mergeWithKey (\_ a b -> Just $ comb a b) (M.map mk) (const M.empty)


-- | Choose between two alternatives with coin chance
flipCoin :: MonadRandom m => a -> a -> m a
flipCoin a1 a2 = uniform [a1, a2]


-- | Crossover on just the connections. Put the fittest map first.
crossConns :: MonadRandom m => Parameters -> Map InnoId ConnGene -> Map InnoId ConnGene ->
              m (Map InnoId ConnGene)
crossConns params m1 m2 = T.sequence $ superLeft flipConn return m1 m2
  where flipConn c1 c2 = do
          if connEnabled c1 && connEnabled c2
            then flipCoin c1 c2
            else do
            c <- flipCoin c1 c2
            roll <- getRandomR (0,1)
            let enabled
                  | roll <= disableChance params = False
                  | otherwise = True
            return c { connEnabled = enabled }


-- | Crossover on just nodes
crossNodes :: MonadRandom m => Map NodeId NodeGene -> Map NodeId NodeGene ->
              m (Map NodeId NodeGene)
crossNodes m1 m2 = T.sequence $ superLeft flipCoin return m1 m2


-- | Crossover. The first argument is the fittest genome.
crossover :: MonadRandom m => Parameters -> Genome -> Genome -> m Genome
crossover params g1 g2 = Genome `liftM` newNodes `ap` newConns `ap` return newNextNode
  where newNextNode = max (nextNode g1) (nextNode g2)
        newConns = crossConns params (connGenes g1) (connGenes g2)
        newNodes = crossNodes (nodeGenes g1) (nodeGenes g2)


-- | Gets differences where they exist
differences :: Map InnoId ConnGene -> Map InnoId ConnGene -> Map InnoId Double
differences = M.mergeWithKey (\_ c1 c2 -> Just $ oneDiff c1 c2) (const M.empty) (const M.empty)
  where oneDiff c1 c2 = abs $ connWeight c1 - connWeight c2


-- | Genetic distance between two genomes
distance :: Parameters -> Genome -> Genome -> Double
distance params g1 g2 = c1 * exFactor + c2 * disFactor + c3 * weightFactor
  where DistParams c1 c2 c3 = distParams params

        conns1 = connGenes g1
        conns2 = connGenes g2
        
        weightDiffs = differences conns1 conns2

        weightFactor = M.foldl (+) 0 weightDiffs / fromIntegral (M.size weightDiffs)

        ids1 = M.keysSet conns1
        ids2 = M.keysSet conns2

        -- | The lower of the top bounds of innovation numbers
        edge = min (S.findMax ids1) (S.findMax ids2)

        -- | Excess and Disjoint
        exJoints = (ids1 `S.difference` ids2) `S.union` (ids2 `S.difference` ids1)

        (excess, disjoint) = S.partition (<= edge) exJoints

        exFactor = fromIntegral $ S.size excess

        disFactor = fromIntegral $ S.size disjoint
