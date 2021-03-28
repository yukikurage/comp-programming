{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE StrictData       #-}
module Main where

import qualified Control.Monad                as Monad
import qualified Control.Monad.Primitive      as Prim
import qualified Control.Monad.ST             as ST
import qualified Data.Bits                    as Bits
import qualified Data.ByteString.Char8        as BS8
import qualified Data.Char                    as Char
import qualified Data.IORef                   as IORef
import qualified Data.List                    as List
import qualified Data.Maybe                   as Maybe
import qualified Data.STRef                   as STRef
import qualified Data.Vector                  as V
import qualified Data.Vector.Algorithms.Merge as VAM
import qualified Data.Vector.Generic          as VG
import qualified Data.Vector.Mutable          as VM
import qualified Data.Vector.Unboxed          as VU
import qualified Data.Vector.Unboxed.Mutable  as VUM

----------
-- Main --
----------

main :: IO ()
main = do
    [m, h] <- getIntList
    putStrLn $ if h `mod` m == 0 then "Yes" else "No"

-------------
-- Library --
-------------

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
readTuple2 = vecToTuple2 . readVector
getTuple2 :: IO (Int, Int)
getTuple2 = readTuple2 <$> BS8.getLine

readTuple3 :: BS8.ByteString -> (Int, Int, Int)
readTuple3 = vecToTuple3 . readVector
getTuple3 :: IO (Int, Int, Int)
getTuple3 = readTuple3 <$> BS8.getLine

vecToTuple2 :: VUM.Unbox b => VU.Vector b -> (b, b)
vecToTuple2 !v = (v VU.! 0, v VU.! 1)
vecToTuple3 :: VUM.Unbox b => VU.Vector b -> (b, b, b)
vecToTuple3 !v = (v VU.! 0, v VU.! 1, v VU.! 2)

getIntMatrix :: Int -> Int -> IO (Matrix Int)
getIntMatrix !m !n = Matrix m n . VU.concat <$> Monad.replicateM m getVector

getStrMatrix :: Int -> Int -> IO (Matrix Char)
getStrMatrix !m !n = Matrix m n . VU.concat <$>
    Monad.replicateM m (VU.fromList . BS8.unpack <$> BS8.getLine)

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
boolToInt !x = if x then 1 else 0

countV :: (Num b, VU.Unbox t) => (t -> Bool) -> VU.Vector t -> b
countV !f = VU.foldr' ((+) . boolToInt . f) 0

count :: (Foldable t1, Num b) => (t2 -> Bool) -> t1 t2 -> b
count !f = foldr ((+) . boolToInt . f) 0

-- | O(√N) 約数を返します
divisor :: Int -> VU.Vector Int
divisor !n = v VU.++ w where
    v = VU.fromList [m |
        m <- [1 ..  floor . sqrt . fromIntegral $ n], n `mod` m == 0]
    w = VU.reverse . VU.map (div n) $ v

-- | O(ln(N)) 二分探索 ソート済みベクトルを要求します
binalySearch :: (Ord a, VUM.Unbox a) => a -> VU.Vector a -> Bool
binalySearch n v = case compare (v VU.! x) n of
    EQ -> True
    LT -> binalySearch n $ VU.drop (x + 1) v
    GT -> binalySearch n $ VU.take x v
    where
        !x = VU.length v `div` 2

-- | 繰り返しの度に第一引数のアクションを評価して
--   Trueになるまで第二引数のアクションを繰り返します
while :: Monad m => m Bool -> m a -> m ()
while !b !a = do
    b' <- b
    if b' then return () else a >> while b a

-----------
-- Queue --
-----------

data Queue pm a = Queue (VUM.MVector pm Int) (VUM.MVector pm a)

-- | O(N) Queueの作成
queue :: (Prim.PrimMonad m, VUM.Unbox a) =>
    Int -> a -> m (Queue (Prim.PrimState m) a)
queue !n !a = Queue <$> VU.thaw (VU.fromList [0, 0]) <*> VUM.replicate n a

-- | O(1)
qcons :: (Prim.PrimMonad m, VUM.Unbox a) =>
    Queue (Prim.PrimState m) a -> a -> m ()
qcons (Queue !header !buf) a = do
    x <- VUM.read header 0
    let !newHead = (x - 1) `mod` VUM.length buf
    VUM.write header 0 newHead
    VUM.write buf newHead a

-- | O(1)
enqueue :: (Prim.PrimMonad m, VUM.Unbox a) =>
    Queue (Prim.PrimState m) a -> a -> m ()
enqueue (Queue !header !buf) a = do
    x <- VUM.read header 1
    VUM.write buf x a
    VUM.write header 1 $ (x + 1) `mod` VUM.length buf

-- | O(1)
dequeue :: (Prim.PrimMonad m, VUM.Unbox a) => Queue (Prim.PrimState m) a -> m a
dequeue (Queue !header !buf) = do
    x <- VUM.read header 0
    VUM.write header 0 $ (x + 1) `mod` VUM.length buf
    VUM.read buf x

-- | O(1)
qhead :: (Prim.PrimMonad m, VUM.Unbox a) => Queue (Prim.PrimState m) a -> m a
qhead (Queue !header !buf) = do
    x <- VUM.read header 0
    VUM.read buf x

-- | O(1)
qempty :: Prim.PrimMonad m => Queue (Prim.PrimState m) a -> m Bool
qempty (Queue !header _) = (==) <$> VUM.read header 0 <*>  VUM.read header 1

-- | O(1)
qlength :: Prim.PrimMonad m => Queue (Prim.PrimState m) a -> m Int
qlength (Queue !header _) = (-) <$> VUM.read header 1 <*>  VUM.read header 0

-----------
-- Graph --
-----------

newtype Graph = Graph (V.Vector [Int]) deriving (Show, Eq)

-- | O(|V| + |E|)? たぶん
graph :: Int -> VU.Vector (Int, Int) -> Graph
graph !n !edges = Graph $ V.modify (f edges) $ V.replicate n [] where
    f :: Prim.PrimMonad m => VU.Vector (Int, Int) ->
        VM.MVector (Prim.PrimState m) [Int] -> m ()
    f !edges !mv = VU.forM_ edges $ \edge ->
        VM.modify mv (snd edge:) $ fst edge

-- | O(|E|) bfs
bfs :: Prim.PrimMonad m => Graph -> Int -> m (V.Vector (Maybe Int))
bfs (Graph !v) !i = do
    visited <- VM.replicate (V.length v) Nothing
    q <- queue (V.length v) 0
    enqueue q i
    VM.write visited i (Just 0)
    while (qempty q) $ do
        x <- dequeue q
        dist <- Maybe.fromJust <$> VM.read visited x
        Monad.forM_ (v V.! x) $ \y -> do
            !connectedDist <- VM.read visited y
            case connectedDist of
                Nothing -> do
                    enqueue q y
                    VM.write visited y . Just $ dist + 1
                _ -> return ()
    V.freeze visited

------------
-- Matrix --
------------

data Matrix a = Matrix Int Int (VU.Vector a) deriving (Show, Eq)
data MMatrix pm a = MMatrix Int Int (VUM.MVector pm a)

-- | O(H * W) 行列の取得
matrix :: VU.Unbox a => (Int, Int) -> a -> Matrix a
matrix (!h, !w) = Matrix h w . VU.replicate (h * w)

-- | O(H * W)
mgenerate :: VUM.Unbox a =>
    (Int, Int) -> ((Int, Int) -> a) -> Matrix a
mgenerate (!h, !w) !f = Matrix h w $
    VU.generate (h * w) $ \x -> f (x `div` w, x `mod` w)

-- | O(1)
mindex :: VUM.Unbox a => Matrix a -> (Int, Int) ->  a
mindex (Matrix !h !w !v) (!i, !j)  = v VU.! (i * w + j)

-- | O(H * W)
mmnew :: (Prim.PrimMonad m, VU.Unbox a) =>
    (Int, Int) -> m (MMatrix (Prim.PrimState m) a)
mmnew (!h, !w) = MMatrix h w <$> VUM.new (h * w)

-- | O(1)
mmread :: (Prim.PrimMonad m, VU.Unbox a) =>
    MMatrix (Prim.PrimState m) a -> (Int, Int) -> m a
mmread (MMatrix _ !w !mv) (!i, !j) = VUM.read mv (i * w + j)

-- | O(1)
mmwrite :: (Prim.PrimMonad m, VU.Unbox a) =>
    MMatrix (Prim.PrimState m) a -> (Int, Int) -> a -> m ()
mmwrite (MMatrix _ !w !mv) (!i, !j) = VUM.write mv (i * w + j)

-- | O(H * W)
mmfreeze :: (VUM.Unbox a, Prim.PrimMonad m) =>
    MMatrix (Prim.PrimState m) a -> m (Matrix a)
mmfreeze (MMatrix !h !w !mv) = Matrix h w <$> VU.freeze mv


--------------
-- DP(2DIM) --
--------------

-- | O(H * W * O(f))
dp2 :: (Prim.PrimMonad m, VUM.Unbox a) => Int -> Int ->
    (MMatrix (Prim.PrimState m) a -> Int -> Int -> m a) -> m (Matrix a)
dp2 !h !w !f = do
    mm <- mmnew (h, w)
    Monad.forM_ [(x, y) | x <- [0 .. h - 1], y <- [0 .. w - 1]] $ \(i, j)-> do
        mmwrite mm (i, j) =<< f mm i j
    mmfreeze mm
