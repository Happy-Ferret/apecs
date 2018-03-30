{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}

module Apecs.System where

import           Control.Monad.Reader
import qualified Data.Vector.Unboxed  as U

import           Apecs.Core

-- | Run a system with a game world
{-# INLINE runSystem #-}
runSystem :: SystemT w m a -> w -> m a
runSystem sys = runReaderT (unSystem sys)

-- | Run a system with a game world
{-# INLINE runWith #-}
runWith :: w -> SystemT w m a -> m a
runWith = flip runSystem

{-# INLINE withStore #-}
withStore :: forall w m c a. Has w m c => (Storage c -> m a) -> SystemT w m a
withStore f = do
  s :: Storage c <- getStore
  lift$ f s

{-# INLINE get #-}
get :: forall w m c. Has w m c => Entity -> SystemT w m c
get (Entity ety) = withStore $ \(s :: Storage c) -> explGet s ety

-- | Writes a component to a given entity. Will overwrite existing components.
--   The type was originally 'Entity c -> c -> System w ()', but is relaxed to 'Entity e'
--   so you don't always have to write 'set . cast'
{-# INLINE set #-}
set :: forall w m c. Has w m c => Entity -> c -> SystemT w m ()
set (Entity ety) x = withStore $ \(s :: Storage c) -> explSet s ety x

-- | Returns whether the given entity has component @c@
--   Note that @c@ is a phantom argument, used only to convey the type of the entity to be queried.
{-# INLINE exists #-}
exists :: forall w m c. Has w m c => Entity -> c -> SystemT w m Bool
exists (Entity ety) ~_ = withStore $ \(s :: Storage c) -> explExists s ety

-- | Maps a function over all entities with a @cx@, and writes their @cy@
{-# INLINE cmap #-}
cmap :: forall w m cx cy. (Has w m cx, Has w m cy)
     => (cx -> cy) -> SystemT w m ()
cmap f = withStore $ \((sx,sy) :: Storage (cx,cy)) -> do
  sl <- explMembers sx
  U.forM_ sl $ \ e -> do
    r <- explGet sx e
    explSet sy e (f r)

-- | Monadically iterates over all entites with a cx
{-# INLINE cmapM #-}
cmapM :: forall w m c a. Has w m c
      => (c -> SystemT w m a) -> SystemT w m [a]
cmapM sys = do
  s :: Storage c <- getStore
  sl <- lift$ explMembers s
  forM (U.toList sl) $ \ ety -> do
    x <- lift$ explGet s ety
    sys x

-- | Monadically iterates over all entites with a cx
{-# INLINE cmapM_ #-}
cmapM_ :: forall w m c a. Has w m c
       => (c -> SystemT w m a) -> SystemT w m ()
cmapM_ sys = do
  s :: Storage c <- getStore
  sl <- lift$ explMembers s
  U.forM_ sl $ \ ety -> do
    x <- lift$ explGet s ety
    sys x

-- | Get all components @c@.
--   Call as @[(c,Entity)]@ to read the entity/index.
{-# INLINE getAll #-}
getAll :: forall w m c. Has w m c
      => SystemT w m [c]
getAll = withStore $ \(s :: Storage c) -> fmap (U.toList) (explMembers s) >>= mapM (explGet s)


-- | Destroys component @c@ for the given entity.
-- Note that @c@ is a phantom argument, used only to convey the type of the entity to be destroyed.
{-# INLINE destroy #-}
destroy :: forall w m c. Has w m c => Entity -> c -> SystemT w m ()
destroy (Entity ety) ~_ = withStore $ \(s :: Storage c) -> explDestroy s ety

-- | Applies a function, if possible.
{-# INLINE modify #-}
modify :: forall w m c. Has w m c => Entity -> (c -> c) -> SystemT w m ()
modify (Entity ety) f = withStore $ \(s :: Storage c) -> do
  e <- explExists s ety
  when e $ explGet s ety >>= explSet s ety . f

-- | Counts the number of entities with a @c@
{-# INLINE count #-}
count :: forall w m c. Has w m c => c -> SystemT w m Int
count ~_ = withStore $ \(s :: Storage c) -> U.length <$> explMembers s
