module Main (main) where

import Actor (A (..), Actor (..), mapRun)
import Control.Monad (forM_)

data Msg
  = Ping
  | Pong
  | End
  deriving (Show, Eq)

newtype Echo = Echo Int

instance Actor Echo Msg where
  run (Echo n) m = case n of
    0 -> (Echo 0, End)
    n ->
      ( Echo $ n - 1,
        case m of
          Ping -> Pong
          Pong -> Ping
          End -> End
      )

world :: Msg -> [A Msg] -> IO ()
world m as = do
  let (as', ms) = mapRun as m
  forM_ ms print
  if End `elem` ms
    then pure ()
    else case m of
      Ping -> world Pong as'
      Pong -> world Ping as'
      End -> pure ()

main :: IO ()
main = do
  world Ping [A $ Echo 10, A $ Echo 10]
  pure ()
