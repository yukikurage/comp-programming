{-# OPTIONS_GHC -O2 #-}

{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NegativeLiterals  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Criterion.Main
import qualified Data.Array.IArray             as A
import qualified Data.Array.Unboxed            as AU
import           Data.Bits
import qualified Data.ByteString.Char8         as BS8
import           Data.Char
import           Data.IORef
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.STRef
import           Data.Semigroup
import qualified Data.Vector                   as V
import           Data.Vector.Algorithms.Merge
import           Data.Vector.Algorithms.Search
import           Data.Vector.Generic           ((!))
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM
import           Debug.Trace

funcExp3 :: Int -> (a -> a) -> a -> a
funcExp3 n = V.foldr1 (.) . V.replicate n

funcExp4 :: Int -> (a -> a) -> a -> a
funcExp4 n f x = v ! n where
    v = V.iterateN (n + 1) f x

funcExp5 :: Int -> (a -> a) -> a -> a
funcExp5 n f x = runST $ do
    ref <- newSTRef x
    VU.forM_ [1 .. n] \_ -> modifySTRef' ref f
    readSTRef ref

main = do
    print $ funcExp3 10000000 f 2
    print $ funcExp4 10000000 f 2
    defaultMain [
        bgroup "funcExp" [
            bench "3" $ nf (funcExp3 10000000 f) 2,
            bench "4" $ nf (funcExp3 10000000 f) 2,
            bench "5" $ nf (funcExp4 10000000 f) 2
            ]
        ]

f :: Int -> Int
f = modMulti 17 2

modMulti m a b = (a * b) `mod` m
