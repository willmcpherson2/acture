module Actor (Actor (..), A (..), mapRun, foldRun) where

import Data.List.NonEmpty (unzip)
import Prelude hiding (unzip)

class Actor s m where
  run :: s -> m -> (s, m)

data A m = forall s. Actor s m => A s

instance Actor (A m) m where
  run (A s) m =
    let (s', m') = run s m
     in (A s', m')

mapRun :: (Actor s m, Functor f) => f s -> m -> (f s, f m)
mapRun s m = unzip $ fmap (`run` m) s

foldRun :: (Actor s m, Foldable t, Applicative f, Monoid (f m)) => s -> t m -> (s, f m)
foldRun s =
  foldr
    (\m (s, ms) -> let (s', m') = run s m in (s', pure m' <> ms))
    (s, mempty)
