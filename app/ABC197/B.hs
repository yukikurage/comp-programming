{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE StrictData       #-}
{-# LANGUAGE TypeOperators    #-}

module Main where

import qualified Control.Monad                as Monad
import qualified Control.Monad.Primitive      as Prim
import qualified Control.Monad.ST             as ST
import qualified Data.Array.IArray            as A
import           Data.Array.Repa              (Z (..), type (:.) (..))
import qualified Data.Array.Repa              as R
import qualified Data.Array.Unboxed           as AU
import qualified Data.Bits                    as Bits
import qualified Data.ByteString.Char8        as BS8
import qualified Data.Char                    as Char
import qualified Data.IORef                   as IORef
import qualified Data.List                    as List
import qualified Data.Maybe                   as Maybe
import qualified Data.STRef                   as STRef
import qualified Data.Vector                  as V
import qualified Data.Vector.Algorithms.Merge as VAM
import           Data.Vector.Generic          ((!))
import qualified Data.Vector.Generic          as VG
import qualified Data.Vector.Mutable          as VM
import qualified Data.Vector.Unboxed          as VU
import qualified Data.Vector.Unboxed.Mutable  as VUM
import qualified Debug.Trace                  as Trace

----------
-- Main --
----------

main :: IO ()
main = do
    [h, w, x, y] <- getIntList
    str <- V.replicateM h $ VU.fromList <$> getLine
    let m = A.listArray ((1, 1), (h, w)) $ [f i | i <- A.range ((1, 1), (h, w))] :: A.Array (Int, Int) Bool
        f (i, j) = (str ! (i - 1)) ! (j - 1) == '.'
    print $ solve h w m (x, y)

solve :: Int -> Int -> A.Array (Int, Int) Bool -> (Int, Int) -> Int
solve h w m (x, y) = 1 + (sum . map f $ [(0, 1), (1, 0), (0, -1), (-1, 0)]) where
    f dir = loop m ((x, y) *+* dir) dir
    loop m pos dir
        | outindex h w pos = 0
        | otherwise = if m A.! pos then loop m (pos *+* dir) dir + 1 else 0

outindex h w (x, y) = x <= 0 || x > h || y <= 0 || y > w
(*+*) (x, y) (z, w) = (x + z, y + w)

-------------
-- Library --
-------------

inf :: Double
inf = 1/0

readDouble :: BS8.ByteString -> Double
readDouble = read . BS8.unpack
getDouble :: IO Double
getDouble = readDouble <$> BS8.getLine

readDoubleList :: BS8.ByteString -> [Double]
readDoubleList = map readDouble . BS8.split ' '
getDoubleList :: IO [Double]
getDoubleList = readDoubleList <$> BS8.getLine

readInt :: BS8.ByteString -> Int
readInt = fst . Maybe.fromJust . BS8.readInt
getInt :: IO Int
getInt = readInt <$> BS8.getLine

readIntList :: BS8.ByteString -> [Int]
readIntList = map readInt . BS8.split ' '
getIntList :: IO [Int]
getIntList = readIntList <$> BS8.getLine

readVector :: BS8.ByteString -> VU.Vector Int
readVector = VU.fromList . readIntList
getVector :: IO (VU.Vector Int)
getVector = readVector <$> BS8.getLine

readTuple2 :: BS8.ByteString -> (Int, Int)
readTuple2 = (\[x, y] -> (x, y)) . readIntList
getTuple2 :: IO (Int, Int)
getTuple2 = readTuple2 <$> BS8.getLine

readTuple3 :: BS8.ByteString -> (Int, Int, Int)
readTuple3 = (\[x, y, z] -> (x, y, z)) . readIntList
getTuple3 :: IO (Int, Int, Int)
getTuple3 = readTuple3 <$> BS8.getLine

vecToTuple2 :: VUM.Unbox b => VU.Vector b -> (b, b)
vecToTuple2 v = (v VU.! 0, v VU.! 1)
vecToTuple3 :: VUM.Unbox b => VU.Vector b -> (b, b, b)
vecToTuple3 v = (v VU.! 0, v VU.! 1, v VU.! 2)

getIntMatrix :: Int -> Int -> IO (R.Array R.U (Z :. Int :. Int) Int)
getIntMatrix m n = R.fromUnboxed (Z :. m :. n) . VU.concat . replicate m <$> getVector

getCharMatrix :: Int -> Int -> IO (R.Array R.U (Z :. Int :. Int) Char)
getCharMatrix m n = R.fromUnboxed (Z :. m :. n) . VU.concat . replicate m . VU.fromList <$> getLine

getTuple2s :: Int -> IO (VU.Vector (Int, Int))
getTuple2s = flip VU.replicateM getTuple2
getTuple3s :: Int -> IO (VU.Vector (Int, Int, Int))
getTuple3s = flip VU.replicateM getTuple3

getIntListLn :: Int -> IO [Int]
getIntListLn = flip Monad.replicateM getInt
getVectorLn :: Int -> IO (VU.Vector Int)
getVectorLn = flip VU.replicateM getInt

-- | 改行なし標準出力
printF :: (Show a) => a -> IO ()
printF = putStr . show

boolToInt :: Num p => Bool -> p
boolToInt x = if x then 1 else 0

countV :: (Num b, VU.Unbox t) => (t -> Bool) -> VU.Vector t -> b
countV f = VU.foldr' ((+) . boolToInt . f) 0

-- | O(√N) 約数を返します
divisor :: Int -> VU.Vector Int
divisor n = v VU.++ w where
    v = VU.fromList [m |
        m <- [1 ..  floor . sqrt . fromIntegral $ n], n `mod` m == 0]
    w = VU.reverse . VU.map (div n) $ v

-- | O(ln(N)) 二分探索 ソート済みベクトルを要求します
binalySearch :: VUM.Unbox a => (t -> a -> Ordering) -> t -> VU.Vector a -> Bool
binalySearch f n v = case f n (v VU.! x) of
    EQ -> True
    GT -> binalySearch f n $ VU.drop (x + 1) v
    LT -> binalySearch f n $ VU.take x v
    where
        x = VU.length v `div` 2

-- | O(ln(N))
lowerBound :: VUM.Unbox a => (t -> a -> Ordering) -> t -> VU.Vector a -> Int
lowerBound f n v = loop f n v 0 $ VU.length v - 1 where
    loop f n v i j
        | i > j = i
        | otherwise = case f n (v VU.! x) of
            GT -> loop f n v (x + 1) j
            _  -> loop f n v i (x - 1)
            where
                x = (i + j) `div` 2

-- | O(ln(N))
upperBound :: VUM.Unbox a => (t -> a -> Ordering) -> t -> VU.Vector a -> Int
upperBound f n v = loop f n v 0 $ VU.length v - 1 where
    loop f n v i j
        | i > j = i
        | otherwise = case f n (v VU.! x) of
            LT -> loop f n v i (x - 1)
            _  -> loop f n v (x + 1) j
            where
                x = (i + j) `div` 2

while :: Monad m => m Bool -> m ()
while a = do
    x <- a
    if x then return () else while a

-----------
-- Queue --
-----------

data Queue pm a = Queue (VUM.MVector pm Int) (VUM.MVector pm a)

-- | O(N) Queueの作成
queue :: (Prim.PrimMonad m, VUM.Unbox a) =>
    Int -> a -> m (Queue (Prim.PrimState m) a)
queue n a = Queue <$> VU.thaw (VU.fromList [0, 0]) <*> VUM.replicate n a

-- | O(1)
qcons :: (Prim.PrimMonad m, VUM.Unbox a) =>
    Queue (Prim.PrimState m) a -> a -> m ()
qcons (Queue header buf) a = do
    x <- VUM.read header 0
    let newHead = (x - 1) `mod` VUM.length buf
    VUM.write header 0 newHead
    VUM.write buf newHead a

-- | O(1)
enqueue :: (Prim.PrimMonad m, VUM.Unbox a) =>
    Queue (Prim.PrimState m) a -> a -> m ()
enqueue (Queue header buf) a = do
    x <- VUM.read header 1
    VUM.write buf x a
    VUM.write header 1 $ (x + 1) `mod` VUM.length buf

-- | O(1)
dequeue :: (Prim.PrimMonad m, VUM.Unbox a) => Queue (Prim.PrimState m) a -> m a
dequeue (Queue header buf) = do
    x <- VUM.read header 0
    VUM.write header 0 $ (x + 1) `mod` VUM.length buf
    VUM.read buf x

-- | O(1)
qhead :: (Prim.PrimMonad m, VUM.Unbox a) => Queue (Prim.PrimState m) a -> m a
qhead (Queue header buf) = do
    x <- VUM.read header 0
    VUM.read buf x

-- | O(1)
qempty :: Prim.PrimMonad m => Queue (Prim.PrimState m) a -> m Bool
qempty (Queue header _) = (==) <$> VUM.read header 0 <*>  VUM.read header 1

-- | O(1)
qlength :: Prim.PrimMonad m => Queue (Prim.PrimState m) a -> m Int
qlength (Queue header _) = (-) <$> VUM.read header 1 <*>  VUM.read header 0

-----------
-- Graph --
-----------

newtype Graph = Graph (V.Vector [Int]) deriving (Show, Eq)

-- | O(|V| + |E|)? たぶん
graph :: Int -> VU.Vector (Int, Int) -> Graph
graph n edges = Graph $ V.modify (f edges) $ V.replicate n [] where
    f :: Prim.PrimMonad m => VU.Vector (Int, Int) ->
        VM.MVector (Prim.PrimState m) [Int] -> m ()
    f edges mv = VU.forM_ edges $ \edge ->
        VM.modify mv (snd edge:) $ fst edge

-- | O(|E|) bfs

bfs :: Prim.PrimMonad m => Graph -> Int -> m (V.Vector (Maybe Int))
bfs (Graph v) i = do
    visited <- VM.replicate (V.length v) Nothing
    q <- queue (V.length v) 0
    enqueue q i
    VM.write visited i (Just 0)
    while $ do
        x <- dequeue q
        dist <- Maybe.fromJust <$> VM.read visited x
        Monad.forM_ (v V.! x) $ \y -> do
            connectedDist <- VM.read visited y
            case connectedDist of
                Nothing -> do
                    enqueue q y
                    VM.write visited y . Just $ dist + 1
                _ -> return ()
        qempty q
    V.freeze visited
