{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Yield where

class Monad m => MonadYield b m | m -> b where
  yield :: b -> m ()
