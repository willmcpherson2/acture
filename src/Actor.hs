module Actor (Actor (..), A (..), runAll) where

class Actor s m where
  run :: s -> [m] -> (s, [m])

data A m = forall s. Actor s m => A s

instance Actor (A m) m where
  run (A s) m =
    let (s', m') = run s m
     in (A s', m')

runAll :: Actor s m => [s] -> [m] -> ([s], [m])
runAll ss ms =
  let (ss', ms') = unzip $ map (`run` ms) ss
   in (ss', concat ms')
