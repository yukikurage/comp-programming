{-# LANGUAGE FlexibleContexts#-}

module Main where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Algorithms.Merge as VAM
import qualified Data.Vector.Generic as VG
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Maybe as Maybe
import qualified Data.Array.Repa as R
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Control.Monad as Monad

----------------
-- Main
----------------

main :: IO ()
main = do
    [a, b, c] <- getIntList
    putStrLn $ case compare a b of
        LT -> "Aoki"
        GT -> "Takahashi"
        EQ -> if c == 0 then "Aoki" else "Takahashi"

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

getMatrix :: Int -> Int -> IO (R.Array R.U R.DIM2 Int)
getMatrix m n = R.fromUnboxed (R.Z R.:. m R.:. n) . VU.concat <$> 
    (replicate m <$> getVector)

getListIntLn :: Int -> IO [Int]
getListIntLn n = replicateMN n getInt
getVectorLn :: Int -> IO (VU.Vector Int)
getVectorLn n = VU.replicateM n getInt

-- | モナド的なreplicate (リスト用)
replicateMN :: Monad m => Int -> m a -> m [a]
replicateMN n m = sequence . take n $ replicate n m

-- | 改行なし標準出力
printF :: (Show a) => a -> IO ()
printF = putStr . show

countV :: (VG.Vector v t, Num b) => (t -> Bool) -> v t -> b
countV f = VG.foldr' (\x -> if f x then (+ 1) else id) 0

count :: (Foldable t1, Num b) => (t2 -> Bool) -> t1 t2 -> b
count f = foldr (\x -> if f x then (+ 1) else id) 0

-- | VectorからNothingを排除して返す。Just a がなければNothingを返す
-- | (リストラ候補)
justFilter :: (VG.Vector v b, VG.Vector v (Maybe b)) => v (Maybe b) -> Maybe (v b)
justFilter v = case VG.filter Maybe.isJust v of
    w | VG.length w == 0 -> Nothing
    w -> Just $ VG.map Maybe.fromJust w
