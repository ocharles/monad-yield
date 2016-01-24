{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Criterion
import Criterion.Main
import Prelude hiding (drop, map, filter, take)

import qualified Inline
import qualified Transformers
import qualified TransformersInline

--------------------------------------------------------------------------------
-- Benchmarks!
value :: Int
value = 1000000

main :: IO ()
main =
  defaultMain
    [bgroup "map"
            [bench "inline" (whnf Inline.benchMap value)
            ,bench "transformers" (whnf Transformers.benchMap value)
            ,bench "transformers+inline" (whnf TransformersInline.benchMap value)]
    ,bgroup "drop"
            [bench "inline" (whnf Inline.benchDrop value)
            ,bench "transformers" (whnf Transformers.benchDrop value)
            ,bench "transformers-inline" (whnf TransformersInline.benchDrop value)]
    ,bgroup "map . drop . map"
            [bench "inline" (whnf Inline.benchMapDropMap value)
            ,bench "transformers" (whnf Transformers.benchMapDropMap value)
            ,bench "transformers-inline" (whnf TransformersInline.benchMapDropMap value)]]
