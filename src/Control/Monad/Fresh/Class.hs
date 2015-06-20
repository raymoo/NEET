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

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Control.Monad.Fresh.Class
Description : Monads that provide fresh values
Copyright   : (c) Leon Medvinsky, 2015

License     : GPL-3
Maintainer  : lmedvinsky@hotmail.com
Stability   : experimental
Portability : portable
-}

module Control.Monad.Fresh.Class (MonadFresh(..)) where



import Control.Monad


-- | A class for monads that can give fresh values.
--
-- At least for Eq s, an instance of 'MonadFresh' should satisfy
--
-- @
-- ('==') '<$>' 'fresh' '<*>' 'fresh' ≡ 'fresh' '*>' 'fresh' '*>' 'pure' 'False'
-- @
--
-- and something similar for any number of fresh.
class Monad m => MonadFresh s m | m -> s where
  fresh :: m s


nFresh :: MonadFresh s m => Int -> m [s]
nFresh n = replicateM n fresh
