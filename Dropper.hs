module Dropper where

import Yield

newtype Dropper a m = Dropper (a -> m (Dropper a m))

dropper :: (MonadYield a m) => Int -> Dropper a m
dropper n
  | n <= 0 = yielder
  | otherwise = Dropper (\_ -> pure (dropper (n - 1)))

yielder :: (MonadYield a m) => Dropper a m
yielder = Dropper (\a -> yielder <$ yield a)
