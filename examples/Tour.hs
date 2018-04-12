{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

import           Apecs
import           Control.Monad

import           Data.Maybe    (maybe)

newtype Position = Position Double deriving Show
newtype Velocity = Velocity Double deriving Show
data Flying = Flying deriving Show

makeWorldAndComponents "World"
  [ ''Position
  , ''Velocity
  , ''Flying
  ]

type System' c = System World c

makeTable :: System' [(Entity,String,String,String)]
makeTable = forM [0..9] $ \e -> do
  (mp,mv,mf) :: (Maybe Position, Maybe Velocity, Maybe Flying) <- get (Entity e)
  return (Entity e, mshow mp, mshow mv, mshow mf)
  where
    mshow = maybe "-" show

printTable :: System' ()
printTable = makeTable >>= liftIO . mapM_ print

game :: System' ()
game = do
  -- Set
  set 0 (Position 1)
  set 1 (Velocity 2)
  set 0 (Position 0)
  -- Get
  Position p <- get 0
  Velocity v <- get 1
  Velocity v <- get 2 -- unsafe
  -- Exists
  e :: Bool <- exists 0 (undefined :: Position)
  -- destroy
  destroy 0 (undefined :: Position)
  e :: Bool <- exists 0 (undefined :: Position)
  -- members
  etys <- members (undefined :: Position)

  printTable
  return ()

main :: IO ()
main = initWorld >>= runSystem game
