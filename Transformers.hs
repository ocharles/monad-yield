{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}

module Transformers where

import "transformers" Control.Monad.Trans.Reader
import "transformers" Control.Monad.Trans.State.Lazy

#include "TransformersImpl.hs"
