import Prelude hiding (map, drop)

import Data.Functor.Identity
import Data.Foldable (traverse_)
import Dropper
import Yield
import Control.Monad

--------------------------------------------------------------------------------

newtype ForT b m a = ForT (ReaderT (b -> m ()) m a)
  deriving (Functor,Applicative,Monad)

instance Monad m => MonadYield b (ForT b m) where
  yield b = ForT (ReaderT (\yield' -> yield' b))

for :: Functor m => ForT b m a -> (b -> m x) -> m a
for (ForT (ReaderT m)) = m . fmap void

map :: (MonadYield b m) => (a -> b) -> ForT a m r -> m r
map f in_ = for in_ (yield . f)

--------------------------------------------------------------------------------

newtype DropT a m r =
  DropT (StateT (Dropper a m) m r)
  deriving (Functor,Applicative,Monad)

drop :: (MonadYield a m)
     => Int -> DropT a m r -> m r
drop n (DropT (StateT s)) = fmap fst (s (dropper n))

instance (Monad m,MonadYield a m) => MonadYield a (DropT a m) where
  yield x =
    DropT (StateT (\(Dropper d) ->
                     do dropper' <- d x
                        return ((),dropper')))

#include "Benchmarks.hs"
