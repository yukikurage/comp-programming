{-# OPTIONS_GHC -O2 #-}

{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Main where
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import qualified Data.Array.IArray             as A
import qualified Data.Array.Unboxed            as AU
import           Data.Bits
import qualified Data.ByteString.Char8         as BS8
import           Data.Char
import           Data.IORef
import           Data.List
import           Data.Maybe
import           Data.Proxy
import           Data.STRef
import qualified Data.Vector                   as V
import           Data.Vector.Algorithms.Merge
import           Data.Vector.Algorithms.Search
import           Data.Vector.Generic           ((!))
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM
import           Debug.Trace
import           GHC.TypeNats

----------
-- Main --
----------

primes :: VU.Vector Int
primes = runST $ do
    v <- VUM.replicate 1001 True
    VUM.write v 0 False
    VUM.write v 1 False
    VU.forM_ [2 .. 1000] \i -> do
        x <- VUM.read v i
        when x $ VU.forM_ [2 * i, 3 * i .. 1000] \j -> VUM.write v j False
    w <- VU.freeze v
    return $ VU.filter (w !) [2 .. 1000]
m = 10 ^ 9 + 7

maxdiv n p = VU.sum $ VU.map
    do \x -> n `div` (p ^ x)
    do [1 .. pow] where
        pow = fromJust $ VU.find
            do \x -> p ^ x > n
            do [1 .. 10 :: Int]

main :: IO ()
main = do
    n <- get @Int
    print . VU.foldr1' modProd $ VU.map
        do \p -> 1 + maxdiv n p
        do primes
    return ()

modProd x y = (x * y) `mod` m

-------------
-- Library --
-------------

class Readable a where
    fromBS :: BS8.ByteString -> a

get :: Readable a => IO a
get = fromBS <$> BS8.getLine

getLn :: (Readable a, VU.Unbox a) => Int -> IO (VU.Vector a)
getLn n = VU.replicateM n get

instance Readable Int where
    fromBS = fst . fromMaybe do error "Error : fromBS @Int"
        . BS8.readInt

instance Readable Double where
    fromBS = read . BS8.unpack

instance (Readable a, Readable b) => Readable (a, b) where
    fromBS bs = (fromBS x0, fromBS x1) where
        [x0, x1] = BS8.split ' ' bs

instance (Readable a, Readable b, Readable c) => Readable (a, b, c) where
    fromBS bs = (fromBS x0, fromBS x1, fromBS x2) where
        [x0, x1, x2] = BS8.split ' ' bs

instance (Readable a, VU.Unbox a) => Readable (VU.Vector a) where
    fromBS = VU.fromList . map fromBS . BS8.split ' '

instance (Readable a) => Readable [a] where
    fromBS = map fromBS . BS8.split ' '
