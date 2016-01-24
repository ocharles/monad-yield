{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}

module TransformersInline where

import "transformers-ocharles" Control.Monad.Trans.Reader
import "transformers-ocharles" Control.Monad.Trans.State.Lazy

#include "TransformersImpl.hs"
