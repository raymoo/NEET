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

module Neet.Parameters (Parameters(..)) where


-- | The genetic parameters
data Parameters =
  Parameters { mutWeightRate  :: Double -- ^ How often weights are mutated
             , newWeightRate  :: Double -- ^ How often weights are replaced if mutated
             , pertAmount     :: Double -- ^ Max amount of perturbation
             , addConnRate    :: Double -- ^ How often new connections are made
             , addNodeRate    :: Double -- ^ How often new nodes are added
             , disableChance  :: Double -- ^ How likely that a disabled parent results
                                        -- in a disabled child
             } 
