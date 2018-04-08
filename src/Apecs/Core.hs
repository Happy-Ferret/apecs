{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module Apecs.Core where

import           Control.Monad.Reader
import           Data.Functor.Identity
import qualified Data.Vector.Unboxed   as U

-- import qualified Apecs.THTuples        as T

-- | An Entity is really just an Int in a newtype, used to index into a component store.
newtype Entity = Entity {unEntity :: Int} deriving (Num, Eq, Ord, Show)

-- | A system is a newtype around `ReaderT w IO a`, where `w` is the game world variable.
newtype SystemT w m a = System {unSystem :: ReaderT w m a} deriving (Functor, Monad, Applicative, MonadIO, MonadTrans)
deriving instance Monad m => MonadReader w (SystemT w m)

-- | A component is defined by the type of its storage
--   The storage in turn supplies runtime types for the component.
--   For the component to be valid, its Storage must be an instance of Store.
class (Elem (Storage c) ~ c) => Component c where
  type Storage c

-- | A world `Has` a component if it can produce its Storage
class (Component c, Monad m, Store m (Storage c)) => Has w m c where
  getStore :: SystemT w m (Storage c)

type family Elem s

-- | Holds components indexed by entities
--
--   Laws:
--
--      * For all entities in @exmplMembers s@, @explExists s ety@ must be true.
--
--      * If for some entity @explExists s ety@, @explGet s ety@ should safely return a non-bottom value.
class Monad m => Store m s where
  -- | Initialize the store with its initialization arguments.
  initStore :: m s

  -- | Writes a component
  explSet :: s -> Int -> Elem s -> m ()
  -- | Reads a component from the store. What happens if the component does not exist is left undefined.
  explGet :: s -> Int -> m (Elem s)
  -- | Destroys the component for a given index.
  explDestroy :: s -> Int -> m ()
  -- | Returns an unboxed vector of member indices
  explMembers :: s -> m (U.Vector Int)

  -- | Returns whether there is a component for the given index
  explExists :: s -> Int -> m Bool
  explExists s n = U.elem n <$> explMembers s

instance Component c => Component (Identity c) where
  type Storage (Identity c) = Identity (Storage c)

instance Has w m c => Has w m (Identity c) where
  getStore = Identity <$> getStore

type instance Elem (Identity s) = Identity (Elem s)
instance Store m s => Store m (Identity s) where
  initStore = error "Initializing Pseudostore"
  explGet (Identity s) e = Identity <$> explGet s e
  explSet (Identity s) e (Identity x) = explSet s e x
  explExists  (Identity s) = explExists s
  explMembers (Identity s) = explMembers s
  explDestroy (Identity s) = explDestroy s


-- Tuple Instances
type instance Elem (a,b) = (Elem a, Elem b)

instance (Component a, Component b) => Component (a,b) where
  type Storage (a,b) = (Storage a, Storage b)
instance (Has w m a, Has w m b) => Has w m (a,b) where
  getStore = (,) <$> getStore <*> getStore
instance (Store m a, Store m b) => Store m (a,b) where
  initStore = (,) <$> initStore <*> initStore
  explGet (sa,sb) ety = (,) <$> explGet sa ety <*> explGet sb ety
  explSet (sa,sb) ety (xa,xb) = explSet sa ety xa >> explSet sb ety xb
  explExists (sa,sb) ety = (&&) <$> explExists sa ety <*> explExists sb ety
  explMembers (sa,sb) = explMembers sa >>= U.filterM (explExists sb)
  explDestroy (sa,sb) ety = explDestroy sa ety >> explDestroy sb ety
-- T.makeInstances [2..8]

-- | Psuedocomponent indicating the absence of @a@.
data Not a = Not

-- | Pseudostore used to produce values of type @Not a@
newtype NotStore s = NotStore s

instance Component c => Component (Not c) where
  type Storage (Not c) = NotStore (Storage c)

instance (Has w m c) => Has w m (Not c) where
  getStore = NotStore <$> getStore

type instance Elem (NotStore s) = Not (Elem s)
instance Store m s => Store m (NotStore s) where
  initStore = error "Initializing Pseudostore"
  explGet _ _ = return Not
  explSet (NotStore sa) ety _ = explDestroy sa ety
  explExists (NotStore sa) ety = not <$> explExists sa ety
  explMembers _ = return mempty
  explDestroy sa ety = explSet sa ety Not

-- | Pseudostore used to produce values of type @Maybe a@
newtype MaybeStore s = MaybeStore s
instance Component c => Component (Maybe c) where
  type Storage (Maybe c) = MaybeStore (Storage c)

instance (Has w m c) => Has w m (Maybe c) where
  getStore = MaybeStore <$> getStore

type instance Elem (MaybeStore s) = Maybe (Elem s)
instance Store m s => Store m (MaybeStore s) where
  initStore = error "Initializing Pseudostore"
  explGet (MaybeStore sa) ety = do
    e <- explExists sa ety
    if e then Just <$> explGet sa ety
         else return Nothing
  explSet (MaybeStore sa) ety Nothing  = explDestroy sa ety
  explSet (MaybeStore sa) ety (Just x) = explSet sa ety x
  explExists _ _ = return True
  explMembers _ = return mempty
  explDestroy (MaybeStore sa) ety = explDestroy sa ety

-- | Pseudostore used to produce components of type @Entity@
data EntityStore = EntityStore
instance Component Entity where
  type Storage Entity = EntityStore

instance Monad m => (Has w m Entity) where
  getStore = return EntityStore

type instance Elem EntityStore = Entity
instance Monad m => Store m EntityStore where
  initStore = error "Initializing Pseudostore"
  explGet _ ety = return $ Entity ety
  explSet _ _ _ = return ()
  explExists _ _ = return True
  explMembers _ = return mempty
  explDestroy _ _ = return ()
