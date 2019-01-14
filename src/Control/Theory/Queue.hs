{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

module Control.Theory.Queue where

import Control.Concurrent.STM
import Control.Monad
import Data.Data
-- import Data.Foldable (Foldable)
-- import Data.Functor.Compose
import Data.ListLike
-- import Data.Time.Clock
-- import Data.Traversable (Traversable)
-- import Data.Typeable
import GHC.Generics
import Prelude hiding (null, reverse, read)

newtype MealyT m f a b = MealyT
  { runMealyT :: a -> m (f b, MealyT m f a b)
  }
  deriving (Typeable, Generic, Functor)

data MooreT m f a b = MooreT
  { _mooreState :: f b
  , _mooreStep  :: a -> m (MooreT m f a b)
  }
  deriving (Typeable, Generic, Functor)

data TSQueue f a = TSQueue
  { _readQueue  :: {-# UNPACK #-} !(TVar (f a))
  , _writeQueue :: {-# UNPACK #-} !(TVar (f a))
  }
  deriving (Typeable, Generic)

flushTQueue :: ListLike (f a) a => TSQueue f a -> STM (f a)
flushTQueue (TSQueue read write) = do
  xs <- readTVar read
  ys <- readTVar write
  unless (null xs) $ writeTVar read empty
  unless (null ys) $ writeTVar write empty
  return (xs `append` reverse ys)

data THQueue h f a = THQueue
  { thqHistory :: h
  , thqCheck   :: a -> h -> Bool
  , thqUpdate  :: a -> h -> h
  , thqQueue   :: TSQueue f a
  }
  deriving (Typeable, Generic)
