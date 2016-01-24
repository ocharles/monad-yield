{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}

module TransformersLocal where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy

#include "TransformersImpl.hs"
