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
Module      : Neet.Training
Description : Generic training abstraction
Copyright   : (c) Leon Medvinsky, 2015

License     : GPL-3
Maintainer  : lmedvinsky@hotmail.com
Stability   : experimental
Portability : ghc
-}

module Neet.Training ( Training(..)
                     , trainSingle
                     , trainTraversable
                     ) where


import qualified Data.Traversable as T



-- | Training structure. The idea is that if it is 'StillTraining', it is
-- presenting you with something that must have its score evaluated, and
-- a way to advance the training by providing that score. If it is 'DoneTraining',
-- everything has been iterated through.
data Training candidate score result =
  StillTraining candidate (score -> Training candidate score result)
  | DoneTraining result


instance (Show c, Show r) => Show (Training c s r) where
  show (DoneTraining res) = "DoneTraining " ++ show res
  show (StillTraining c _) = "StillTraining " ++ show c ++ " <function>"


instance Functor (Training candidate score) where
  fmap f (DoneTraining res) = DoneTraining (f res)
  fmap f (StillTraining cand k) = StillTraining cand ((fmap . fmap) f k)


instance Applicative (Training candidate score) where
  pure = DoneTraining
  (<*>) = apTraining


apTraining :: Training c s (r1 -> r2) -> Training c s r1 -> Training c s r2
apTraining (DoneTraining f) tcsr1 = fmap f tcsr1
apTraining (StillTraining cand k) tcsr1 = StillTraining cand go
  where go score = apTraining (k score) tcsr1


trainSingle :: a -> Training a b b
trainSingle a = StillTraining a k
  where k b = DoneTraining b


trainTraversable :: Traversable t => t a -> Training a b (t b)
trainTraversable = T.traverse trainSingle
