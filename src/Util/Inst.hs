{-# OPTIONS_GHC -cpp -fno-warn-unused-imports #-}
{-# LANGUAGE CPP #-}

-- Useful instances that don't belong anywhere else.
module Util.Inst() where

import Control.Applicative
import Control.Monad.Identity(Identity(..))
import Data.Foldable(Foldable(..))
import Data.Monoid(Monoid(..))
import Data.Traversable(Traversable(..), foldMapDefault, fmapDefault)
import qualified Data.IntMap as IM
import qualified Data.Map as Map

instance Show a => Show (Identity a) where
    show x = show $ runIdentity x

instance Traversable IM.IntMap where
    traverse f mp = (IM.fromAscList . Map.toAscList) `fmap`  (traverse f . Map.fromAscList . IM.toAscList $ mp)

instance Foldable ((,) a) where
    foldMap = foldMapDefault
instance Traversable  ((,) a) where
    traverse f (x,y) = (,) x <$> f y

instance Functor ((,,) a b) where
    fmap = fmapDefault

instance Foldable ((,,) a b) where
    foldMap = foldMapDefault
instance Traversable  ((,,) a b) where
    traverse f (x,y,z) = (,,) x y <$> f z
