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

data THQueue h f a = THQueue
  { thqHistory :: h
  , thqCheck   :: a -> h -> Bool
  , thqUpdate  :: a -> h -> h
  , thqQueue   :: TSQueue f a
  }
  deriving (Typeable, Generic)

-- |Build and returns a new instance of 'TSQueue'
newTSQueue :: ListLike (f a) a => STM (TSQueue f a)
newTSQueue = do
  read  <- newTVar empty
  write <- newTVar empty
  return (TSQueue read write)

-- |@IO@ version of 'newTSQueue'.  This is useful for creating top-level
-- 'TSQueue's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
newTSQueueIO :: ListLike (f a) a => IO (TSQueue f a)
newTSQueueIO = do
  read  <- newTVarIO empty
  write <- newTVarIO empty
  return (TSQueue read write)

-- |Write a value to a 'TSQueue'.
writeTSQueue :: ListLike (f a) a => TSQueue f a -> a -> STM ()
writeTSQueue (TSQueue _read write) a = do
  listend <- readTVar write
  writeTVar write (cons a listend)

-- |Read the next value from the 'TSQueue'.
readTSQueue :: ListLike (f a) a => TSQueue f a -> STM a
readTSQueue (TSQueue read write) = do
  xs <- readTVar read
  case uncons xs of
    Just (x, xs') -> do
      writeTVar read xs'
      return x
    Nothing -> do
      ys <- readTVar write
      if null ys
        then retry
        else do
          -- NB. lazy: we want the transaction to be
          -- short, otherwise it will conflict
          let Just (z, zs) = uncons (reverse ys)
          writeTVar write empty
          writeTVar read zs
          return z

-- | A version of 'readTSQueue' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
tryReadTSQueue :: ListLike (f a) a => TSQueue f a -> STM (Maybe a)
tryReadTSQueue c = fmap Just (readTSQueue c) `orElse` return Nothing

-- | Efficiently read the entire contents of a 'TSQueue' into a list. This
-- function never retries.
--
-- @since 2.4.5
flushTSQueue :: ListLike (f a) a => TSQueue f a -> STM (f a)
flushTSQueue (TSQueue read write) = do
  xs <- readTVar read
  ys <- readTVar write
  unless (null xs) $ writeTVar read empty
  unless (null ys) $ writeTVar write empty
  return (xs `append` reverse ys)

-- | Get the next value from the @TSQueue@ without removing it,
-- retrying if the channel is empty.
peekTSQueue :: ListLike (f a) a => TSQueue f a -> STM a
peekTSQueue c = do
  x <- readTSQueue c
  unGetTSQueue c x
  return x

-- | A version of 'peekTSQueue' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
tryPeekTSQueue :: ListLike (f a) a => TSQueue f a -> STM (Maybe a)
tryPeekTSQueue c = do
  m <- tryReadTSQueue c
  case m of
    Nothing -> return Nothing
    Just x  -> do
      unGetTSQueue c x
      return m

-- |Put a data item back onto a channel, where it will be the next item read.
unGetTSQueue :: ListLike (f a) a => TSQueue f a -> a -> STM ()
unGetTSQueue (TSQueue read _write) a = do
  xs <- readTVar read
  writeTVar read (cons a xs)

-- |Returns 'True' if the supplied 'TSQueue' is empty.
isEmptyTSQueue :: ListLike (f a) a => TSQueue f a -> STM Bool
isEmptyTSQueue (TSQueue read write) = do
  xs <- readTVar read
  if null xs
    then do
      ys <- readTVar write
      return $ null ys
    else return False
