{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}

module Inline where
import Prelude hiding (map, drop)

import Data.Foldable (traverse_)
import Dropper
import Control.Monad
import Yield

newtype ForT b m a = ForT ((b -> m ()) -> m a)

instance Monad m => Functor (ForT b m) where
  fmap = liftM

instance Monad m => Applicative (ForT b m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (ForT b m) where
  return = ForT . const . return
  ForT m >>= f = ForT (\x -> do a <- m x
                                case f a of ForT g -> g x)

instance Monad m => MonadYield b (ForT b m) where
  yield b = ForT (\yield' -> yield' b)

for :: Functor m => ForT b m a -> (b -> m x) -> m a
for (ForT m) = m . fmap void

map :: (MonadYield b m) => (a -> b) -> ForT a m r -> m r
map f in_ = for in_ (yield . f)

newtype DropT a m r = DropT (Dropper a m -> m (r, Dropper a m))

instance Monad m => Functor (DropT a m) where
  fmap = liftM

instance Monad m => Applicative (DropT a m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (DropT a m) where
  return a = DropT (\s -> return (a, s))
  DropT a >>= f = DropT (\s -> do ~(x, s') <- a s
                                  case f x of
                                    DropT f' -> f' s')

instance (Monad m,MonadYield a m) => MonadYield a (DropT a m) where
  yield x =
    DropT (\(Dropper d) ->
                     do dropper' <- d x
                        return ((),dropper'))

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where fmap = liftM

instance Applicative Identity where
  pure = return
  (<*>) = ap

instance Monad Identity where
  return = Identity
  Identity a >>= f = f a

drop :: (MonadYield a m)
     => Int -> DropT a m r -> m r
drop n (DropT s) = fmap fst (s (dropper n))

#include "Benchmarks.hs"
