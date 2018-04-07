{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeFamilies               #-}

module Apecs.Util (
  -- * Utility
  runGC, global, proxy,

  -- * EntityCounter
  EntityCounter, nextEntity, newEntity,

  -- * Spatial hashing
  -- $hash
  quantize, flatten, inbounds, region, flatten',

  -- * Timing
  timeSystem, timeSystem_,

  ) where

import           Control.Applicative  (liftA2)
import           Control.Monad.Reader (liftIO)
import           Control.Monad.IO.Class
import           Data.Monoid
import           System.CPUTime
import           System.Mem           (performMajorGC)

import           Apecs.Stores
import           Apecs.System
import           Apecs.Core

-- | Convenience entity (-1), used in places where the exact entity value does not matter, i.e. a global store.
global :: Entity
global = Entity (-1)

-- | Convenience proxy value
proxy :: forall t. t
proxy = error "Proxy value"

-- | Component used by newEntity to track the number of issued entities.
--   Automatically added to any world created with @makeWorld@
newtype EntityCounter = EntityCounter {getCounter :: Sum Int} deriving (Monoid, Eq, Show)

instance Component EntityCounter where
  type Storage EntityCounter = Global IO EntityCounter

-- | Bumps the EntityCounter and yields its value
{-# INLINE nextEntity #-}
nextEntity :: Has w IO EntityCounter => SystemT w IO Entity
nextEntity = do EntityCounter n <- get global
                set global (EntityCounter $ n+1)
                return (Entity . getSum $ n)

-- | Writes the given components to a new entity, and yields that entity.
-- The return value is often ignored.
{-# INLINE newEntity #-}
newEntity :: (Store IO (Storage c), Has w IO c, Has w IO EntityCounter)
          => c -> SystemT w IO Entity
newEntity c = do ety <- nextEntity
                 set ety c
                 return ety

-- | Explicitly invoke the garbage collector
runGC :: MonadIO m => SystemT w m ()
runGC = liftIO performMajorGC

-- $hash
-- The following are helper functions for spatial hashing.
-- Your spatial hash is defined by two vectors;
--
--   - The cell size vector contains real components and dictates
--     how large each cell in your table is in world space units.
--     It is used by @quantize@ to translate a world space coordinate into a table space index vector
--   - The table size vector contains integral components and dictates how
--     many cells your field consists of in each direction.
--     It is used by @flatten@ to translate a table-space index vector into a flat integer

-- | Quantize turns a world-space coordinate into a table-space coordinate by dividing
--   by the given cell size and rounding towards negative infinity.
{-# INLINE quantize #-}
quantize :: (Fractional (v a), Integral b, RealFrac a, Functor v)
         => v a -- ^ Quantization cell size
         -> v a -- ^ Vector to be quantized
         -> v b
quantize cell vec = floor <$> vec/cell

-- | Turns a table-space vector into an integral index, given some table size vector.
--   Yields Nothing for out-of-bounds queries
{-# INLINE flatten #-}
flatten :: (Applicative v, Integral a, Foldable v)
        => v a -- Field size vector
        -> v a -> Maybe a
flatten size vec = if inbounds size vec then Just (flatten' size vec) else Nothing

-- | Tests whether a vector is in the region given by 0 and the size vector (inclusive)
{-# INLINE inbounds #-}
inbounds :: (Num a, Ord a, Applicative v, Foldable v)
         => v a -- Field size vector
         -> v a -> Bool
inbounds size vec = and (liftA2 (\v s -> v >= 0 && v <= s) vec size)

-- | For two table-space vectors indicating a region's bounds, gives a list of the vectors contained between them.
--   This is useful for querying a spatial hash.
{-# INLINE region #-}
region :: (Enum a, Applicative v, Traversable v)
       => v a -- ^ Lower bound for the region
       -> v a -- ^ Higher bound for the region
       -> [v a]
region a b = sequence $ liftA2 enumFromTo a b

-- | flatten, but yields garbage for out-of-bounds vectors.
{-# INLINE flatten' #-}
flatten' :: (Applicative v, Integral a, Foldable v)
            => v a -- Field size vector
            -> v a -> a
flatten' size vec = foldr (\(n,x) acc -> n*acc + x) 0 (liftA2 (,) size vec)

-- | Runs a system and gives its execution time in seconds
timeSystem :: MonadIO m => SystemT w m a -> SystemT w m (Double, a)
timeSystem sys = do
  s <- liftIO getCPUTime
  a <- sys
  t <- liftIO getCPUTime
  return (fromIntegral (t-s)/1e12, a)

-- | Runs a system, discards its output, and gives its execution time in seconds
timeSystem_ :: MonadIO m => SystemT w m a -> SystemT w m Double
timeSystem_ = fmap fst . timeSystem
