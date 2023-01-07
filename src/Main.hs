module Main (main) where

import Actor (A (..), Actor (..), runAll)
import Control.Lens

data Pos = Pos {_x :: Int, _y :: Int}
  deriving (Show, Eq)

makeLenses ''Pos

newtype Msg
  = Occupy Pos
  deriving (Show, Eq)

newtype Tree = Tree {_treePos :: Pos}
  deriving (Show, Eq)

makeLenses ''Tree

instance Actor Tree Msg where
  run s _ = (s, [Occupy $ s ^. treePos])

newtype Man = Man {_manPos :: Pos}
  deriving (Show, Eq)

makeLenses ''Man

instance Actor Man Msg where
  run s ms =
    let p = s ^. manPos
        p' = p & x +~ 1
        occupied = Occupy p' `elem` ms
     in if occupied
          then (s, [Occupy p])
          else (s & manPos .~ p', [Occupy p'])

newtype World = World {_n :: Int}

makeLenses ''World

world :: World -> [A Msg] -> [Msg] -> IO (World, [A Msg], [Msg])
world w as ms = do
  print ms
  case w ^. n of
    0 -> pure (w, as, ms)
    _ -> do
      let (as', ms') = runAll as ms
      world (w & n -~ 1) as' ms'

main :: IO ()
main = do
  _ <-
    world
      World {_n = 10}
      [ A $ Man {_manPos = Pos 0 0},
        A $ Tree {_treePos = Pos 5 0}
      ]
      []
  pure ()
