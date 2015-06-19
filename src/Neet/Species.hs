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

module Neet.Species () where


import Neet.Genome
import Data.MultiMap (MultiMap)
import qualified Data.MultiMap as MM


-- ^ A NEAT Species.
data Species =
  Species { specSize :: Int
          , specOrgs :: MultiMap Double Genome -- ^ Map from fitnesses to genomes
          , bestScore :: Double                -- ^ Best unadjusted fitness so far
          , bestGen :: Genome                  -- ^ Best genome so far
          , lastImprovement :: Int             -- ^ Number of gens ago the best score improved
          } 

instance Show Species where
  show (Species siz _ scr bestGen lastImprov) =
    "Species {specSize = " ++ show siz ++
    ", specOrgs = <...>, bestScore = " ++ show scr ++
    ", bestGen = <...>" ++
    ", lastImprovement = " ++ show lastImprov ++ "}"

