{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Apecs.Stores
  ( Map, Global
  ) where

import           Control.Monad.Reader
import           Control.Monad.Primitive
import           Control.Monad.ST
import qualified Data.IntMap.Strict          as M
import           Data.Maybe                  (fromJust)
import           Data.Proxy
import qualified Data.Vector.Mutable         as VM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           GHC.TypeLits

import           Apecs.Core

-- | A map based on @Data.Intmap.Strict@. O(log(n)) for most operations.
newtype Map m c = Map (VM.MVector (PrimState m) (M.IntMap c))
type instance Elem (Map s c) = c

type IOMap c = Map IO c
type STMap s c = Map (ST s) c

instance (PrimMonad m) => Store m (Map m c) where
  initStore = Map <$> VM.replicate 1 (mempty :: M.IntMap c)
  explGet     (Map ref) ety   = fromJust . M.lookup ety <$> VM.read ref 0
  explSet     (Map ref) ety x = VM.modify ref (M.insert ety x) 0
  explExists  (Map ref) ety   = M.member ety <$> VM.read ref 0
  explDestroy (Map ref) ety   = VM.modify ref (M.delete ety) 0
  explMembers (Map ref)       = U.fromList . M.keys <$> VM.read ref 0
  {-# INLINE explGet #-}
  {-# INLINE explSet #-}
  {-# INLINE explDestroy #-}
  {-# INLINE explMembers #-}
  {-# INLINE explExists #-}

-- | A Global contains exactly one component.
--   Initialized with 'mempty'
--   The store will return true for every existence check, but only ever gives (-1) as its inhabitant.
--   The entity argument is ignored when setting/getting a global.
newtype Global m c = Global (VM.MVector (PrimState m) c)
type instance Elem (Global m c) = c
instance (PrimMonad m, Monoid c) => Store m (Global m c) where
  initStore = Global <$> VM.replicate 1 mempty
  explGet     (Global ref) ety   = VM.read ref 0
  explSet     (Global ref) ety x = VM.write ref 0 x
  explExists  _ _                = return True
  explDestroy _ _                = return ()
  explMembers (Global ref)       = return $ U.singleton 1
  {-# INLINE explDestroy #-}
  {-# INLINE explMembers #-}
  {-# INLINE explExists #-}
  {-# INLINE explSet #-}
  {-# INLINE explGet #-}

{--
-- | A cache around another store.
--   Note that iterating over a cache is linear in cache size, so sparsely populated caches might actually decrease performance.
data Cache (n :: Nat) s =
  Cache Int (UM.IOVector Int) (VM.IOVector (Elem s)) s

-- | An empty type class indicating that the store behaves like a regular map, and can therefore safely be cached.
class Cachable s
instance Cachable (Map s)
instance (KnownNat n, Cachable s) => Cachable (Cache n s)

type instance Elem (Cache n s) = Elem s
instance (KnownNat n, Cachable s) => Store IO (Cache n s) where
  initStore = do
    let n = fromIntegral$ natVal (Proxy @n)
    tags <- UM.replicate n (-1)
    cache <- VM.new n
    child <- initStore
    return (Cache n tags cache child)

  {-# INLINE explDestroy #-}
  explDestroy (Cache n tags _ s) ety = do
    tag <- UM.unsafeRead tags (ety `rem` n)
    if tag == ety
       then UM.unsafeWrite tags (ety `rem` n) (-1)
       else explDestroy s ety

  {-# INLINE explExists #-}
  explExists (Cache n tags _ s) ety = do
    tag <- UM.unsafeRead tags (ety `rem` n)
    if tag == ety then return True else explExists s ety

  {-# INLINE explMembers #-}
  explMembers (Cache _ tags _ s) = do
    cached <- U.filter (/= (-1)) <$> U.freeze tags
    stored <- explMembers s
    return $! cached U.++ stored

  {-# INLINE explGet #-}
  explGet (Cache n tags cache s) ety = do
    let index = ety `rem` n
    tag <- UM.unsafeRead tags index
    if tag == ety
       then VM.unsafeRead cache index
       else explGet s ety

  {-# INLINE explSet #-}
  explSet (Cache n tags cache s) ety x = do
    let index = ety `rem` n
    tag <- UM.unsafeRead tags index
    when (tag /= (-1) && tag /= ety) $ do
      cached <- VM.unsafeRead cache index
      explSet s tag cached
    UM.unsafeWrite tags  index ety
    VM.unsafeWrite cache index x
--}
