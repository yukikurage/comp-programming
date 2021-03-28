module Main where

import qualified Control.Monad                as Monad
import qualified Control.Monad.Primitive      as Prim
import qualified Control.Monad.ST             as ST
import qualified Data.Array.Repa              as R
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

----------------
-- Main
----------------

main :: IO ()
main = do
    [n, x] <- getIntList
    a <- getVector
    let b = VU.filter (/= x) a
    Monad.when (VU.length b >= 1) $ do
        printF (VU.head b)
        VU.forM_ (VU.tail b) $ \i -> do
            putStr " "
            printF i
    putStrLn ""

----------------
-- Library
----------------

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
vecToTuple2 v = (v VU.! 0, v VU.! 1)
vecToTuple3 :: VUM.Unbox b => VU.Vector b -> (b, b, b)
vecToTuple3 v = (v VU.! 0, v VU.! 1, v VU.! 2)

getMatrix :: Int -> Int -> IO (Matrix Int)
getMatrix m n = do
    a <- Monad.replicateM m getVector
    let b = VU.concat a
    return $ Matrix m n b

getTuple2s :: Int -> IO (VU.Vector (Int, Int))
getTuple2s n = VU.replicateM n getTuple2
getTuple3s :: Int -> IO (VU.Vector (Int, Int, Int))
getTuple3s n = VU.replicateM n getTuple3

getListIntLn :: Int -> IO [Int]
getListIntLn n = Monad.replicateM n getInt
getVectorLn :: Int -> IO (VU.Vector Int)
getVectorLn n = VU.replicateM n getInt

-- | 改行なし標準出力
printF :: (Show a) => a -> IO ()
printF = putStr . show

countV :: (Num b, VU.Unbox t) => (t -> Bool) -> VU.Vector t -> b
countV f = VU.foldr' (\x -> if f x then (+ 1) else id) 0

count :: (Foldable t1, Num b) => (t2 -> Bool) -> t1 t2 -> b
count f = foldr (\x -> if f x then (+ 1) else id) 0

-- | O(√N) 約数を返します
divisor :: Int -> VU.Vector Int
divisor n = v VU.++ w where
    v = VU.fromList [m | m <- [1 ..  floor .sqrt . fromIntegral $ n], n `mod` m == 0]
    w = VU.reverse . VU.map (div n) $ v

-- | O(ln(N)) 二分探索 ソート済みベクトルを要求します
binalySearch :: (Ord a, VUM.Unbox a) => a -> VU.Vector a -> Bool
binalySearch n v = case compare (v VU.! x) n of
    EQ -> True
    LT -> binalySearch n $ VU.drop (x + 1) v
    GT -> binalySearch n $ VU.take x v
    where
        x = VU.length v `div` 2

while :: Monad m => m Bool -> m a -> m ()
while b a = do
    b' <- b
    if b' then return () else do
        a
        while b a

----------------
-- Queue
----------------

data Queue pm a = Queue (VUM.MVector pm Int) (VUM.MVector pm a)

-- | O(N) Queueの作成
queue :: (Prim.PrimMonad m, VUM.Unbox a) => Int -> a -> m (Queue (Prim.PrimState m) a)
queue n a = Queue <$> VU.thaw (VU.fromList [0, 0]) <*> VUM.replicate n a

qcons :: (Prim.PrimMonad m, VUM.Unbox a) => Queue (Prim.PrimState m) a -> a -> m ()
qcons (Queue header mv) a = do
    x <- VUM.read header 0
    VUM.write header 0 $ (x - 1) `mod` VUM.length mv
    VUM.write mv ((x - 1) `mod` VUM.length mv) a


enqueue :: (Prim.PrimMonad m, VUM.Unbox a) => Queue (Prim.PrimState m) a -> a -> m ()
enqueue (Queue header mv) a = do
    x <- VUM.read header 1
    VUM.write mv x a
    VUM.write header 1 $ (x + 1) `mod` VUM.length mv

dequeue :: (Prim.PrimMonad m, VUM.Unbox a) => Queue (Prim.PrimState m) a -> m a
dequeue (Queue header mv) = do
    x <- VUM.read header 0
    VUM.write header 0 $ (x + 1) `mod` VUM.length mv
    VUM.read mv x

qhead :: (Prim.PrimMonad m, VUM.Unbox a) => Queue (Prim.PrimState m) a -> m a
qhead (Queue header mv) = do
    x <- VUM.read header 0
    VUM.read mv x

qempty :: Prim.PrimMonad m => Queue (Prim.PrimState m) a -> m Bool
qempty (Queue header mv) = do
    x <- VUM.read header 0
    y <- VUM.read header 1
    return $ x == y

qlength :: Prim.PrimMonad m => Queue (Prim.PrimState m) a -> m Int
qlength (Queue header mv) = do
    x <- VUM.read header 0
    y <- VUM.read header 1
    return $ y - x

----------------
-- Graph
----------------

newtype Graph = Graph (V.Vector [Int]) deriving (Show, Eq)

-- | O(|V| + |E|)? たぶん
graph :: Int -> VU.Vector (Int, Int) -> Graph
graph n edges = Graph $ V.modify (mutableGraphEdgesApply edges) $ V.replicate n []

mutableGraphEdgesApply :: Prim.PrimMonad m => VU.Vector (Int, Int) -> VM.MVector (Prim.PrimState m) [Int] -> m ()
mutableGraphEdgesApply edges mv = do
    VU.forM_ edges $ \edge -> do
        let x = fst edge
            y = snd edge
        VM.modify mv (y:) x
        VM.modify mv (x:) y

-- | O(|E|) bfs
bfs :: Graph -> Int -> VU.Vector Int
bfs (Graph v) i = ST.runST $ do
    visited <- VUM.replicate (V.length v) (-1)
    q <- queue (V.length v) 0
    enqueue q i
    VUM.write visited i 0
    while (qempty q) $ do
        x <- dequeue q
        dist <- VUM.read visited x
        Monad.forM_ (v V.! x) $ \y ->do
            connectedDist <- VUM.read visited y
            case connectedDist of
                -1 -> do
                    enqueue q y
                    VUM.write visited y $ dist + 1
                _ -> return ()
    VU.freeze visited

----------------
-- Matrix
----------------

data Matrix a = Matrix {mheight :: Int, mwidth :: Int, mToVector :: VU.Vector a} deriving (Show, Eq)
data MMatrix pm a = MMatrix {mmheight :: Int, mmwidth :: Int, mmToVector :: VUM.MVector pm a}

-- | 行列の取得 O(M * N)
matrix :: VU.Unbox a => Int -> Int -> a -> Matrix a
matrix m n = Matrix m n . VU.replicate (m * n)

mgenerate :: VUM.Unbox a => Int -> Int -> (Int -> Int -> a) -> Matrix a
mgenerate m n f = Matrix m n $ VU.generate (m * n) $ \x -> f (x `div` n) (x `mod` n)

mindex :: VUM.Unbox a => Matrix a -> Int -> Int ->  a
mindex (Matrix h w v) i j  = v VU.! (i * w + j)

mmnew :: (Prim.PrimMonad m, VU.Unbox a) => Int -> Int -> m (MMatrix (Prim.PrimState m) a)
mmnew h w = MMatrix h w <$> VUM.new (h * w)

mmread :: (Prim.PrimMonad m, VU.Unbox a) => MMatrix (Prim.PrimState m) a -> Int -> Int -> m a
mmread (MMatrix _ w mv) i j = VUM.read mv (i * w + j)

mmwrite :: (Prim.PrimMonad m, VU.Unbox a) => MMatrix (Prim.PrimState m) a -> Int -> Int -> a -> m ()
mmwrite (MMatrix _ w mv) i j = VUM.write mv (i * w + j)

mmfreeze :: (VUM.Unbox a, Prim.PrimMonad m) => MMatrix (Prim.PrimState m) a -> m (Matrix a)
mmfreeze (MMatrix h w mv) = Matrix h w <$> VU.freeze mv

------------------------
-- CompleteBinaryTree
------------------------
data CompleteBinaryTree a = CBT {cbtrank :: Int, cbtfunc :: a -> a -> a, cbtree :: VU.Vector a}
data MCompleteBinaryTree pm a = MCBT {mcbtrank :: Int, mcbtfunc :: a -> a -> a, mcbtree :: VUM.MVector pm a}

newmcbt :: (VUM.Unbox a, Prim.PrimMonad m) => (a -> a -> a) -> Int -> a -> m (MCompleteBinaryTree (Prim.PrimState m) a)
newmcbt f n a = do
    let r = (1 +) . floor . logBase 2 . fromIntegral $ (n - 1)
    mv <- VUM.new (2 ^ (r + 1) - 1)
    Monad.forM_ [2 ^ r - 1 .. 2 ^ (r + 1) - 2] $ \i -> VUM.write mv i a
    Monad.forM_ [2 ^ r - 2, 2 ^ r - 3  .. 0] $ \i -> do
        let (l, r) = cbtnext i
        lv <- VUM.read mv l
        rv <- VUM.read mv r
        VUM.write mv i (f lv rv)
    return $ MCBT r f mv

mcbtwrite :: (VUM.Unbox a, Prim.PrimMonad m) => MCompleteBinaryTree (Prim.PrimState m) a -> Int -> a -> m ()
mcbtwrite mcbt n a = do
    let r = mcbtrank mcbt
    let mv = mcbtree mcbt
    let f = mcbtfunc mcbt
    let tar = 2 ^ r + n - 1
    VUM.write mv tar a
    VU.forM_ (VU.iterateN r ((`div` 2) . (+ (- 1))) ((tar - 1) `div` 2)) $ \i -> do
        let (l, r) = cbtnext i
        lv <- VUM.read mv l
        rv <- VUM.read mv r
        VUM.write mv i (f lv rv)

mcbtfold :: (VUM.Unbox a, Prim.PrimMonad m) => MCompleteBinaryTree (Prim.PrimState m) a -> Int -> m a
mcbtfold mcbt n = do
    let r = mcbtrank mcbt
    let mv = mcbtree mcbt
    let f = mcbtfunc mcbt
    let
        g x 0 z = [x]
        g x y z | (y - 1) `div` z == 0 = g (x * 2 + 1) (y `mod` z) (z `div` 2)
        g x y z = (x * 2 + 1) : g (x * 2 + 2) (y `mod` z) (z `div` 2)
    let is = VU.fromList $ g 0 (n + 1) (2 ^ (r - 1))
    xs <- VU.mapM (VUM.read mv) is
    return $ VU.foldr1' f xs

mcbtToVector :: (VUM.Unbox a, Prim.PrimMonad m) => MCompleteBinaryTree (Prim.PrimState m) a -> m (VU.Vector a)
mcbtToVector mcbt = VU.freeze . VUM.drop (2 ^ mcbtrank mcbt) $ mcbtree mcbt

cbtnext :: Int -> (Int, Int)
cbtnext i = (i * 2 + 1, i * 2 + 2)

----------------
-- DP(2DIM)
----------------

dp2 :: (Prim.PrimMonad m, VUM.Unbox a) => Int -> Int -> (MMatrix (Prim.PrimState m) a -> Int -> Int -> m a) -> m (Matrix a)
dp2 h w f = do
    mm <- mmnew h w
    Monad.forM_ [(x, y) | x <- [0 .. h - 1], y <- [0 .. w - 1]] $ \(i, j)-> do
        mmwrite mm i j =<< f mm i j
    mmfreeze mm
