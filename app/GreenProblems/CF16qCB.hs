{-# OPTIONS_GHC -O2 #-}

{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TypeApplications  #-}

module Main where
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import qualified Data.Array.IArray             as A
import qualified Data.Array.Repa               as R
import qualified Data.Array.Unboxed            as AU
import           Data.Bits
import qualified Data.ByteString.Char8         as BS8
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.STRef
import qualified Data.Vector                   as V
import           Data.Vector.Algorithms.Merge
import           Data.Vector.Algorithms.Search
import           Data.Vector.Generic           ((!))
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM
import           Debug.Trace
import           GHC.Exts

----------
-- Main --
----------

main :: IO ()
main = do
    [k, t] <- get @[Int]
    xs <- get @(VU.Vector Int)
    let x = VU.maximum xs
        y = VU.sum xs
        z = max
            do x - (y - x + 1)
            do 0
    print z

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
