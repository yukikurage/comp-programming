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

readInt :: BS8.ByteString -> Int
readInt = fst . Maybe.fromJust . BS8.readInt
getInt :: IO Int
getInt = readInt <$> BS8.getLine

readListInt :: BS8.ByteString -> [Int]
readListInt = map readInt . BS8.split ' '
getListInt :: IO [Int]
getListInt = readListInt <$> BS8.getLine

readVector :: BS8.ByteString -> VU.Vector Int
readVector = VU.fromList . readListInt
getVector :: IO (VU.Vector Int)
getVector = readVector <$> BS8.getLine

getMatrix :: Int -> Int -> IO (R.Array R.U R.DIM2 Int)
getMatrix m n = R.fromUnboxed (R.Z R.:. m R.:. n) . VU.concat <$> 
    (replicate m <$> getVector)

getListIntLn :: Int -> IO [Int]
getListIntLn n = replicateMN n getInt
getVectorLn :: Int -> IO (VU.Vector Int)
getVectorLn n = VU.replicateM n getInt

replicateMN :: Monad m => Int -> m a -> m [a]
replicateMN n m = sequence . take n $ replicate n m

printF :: (Show a) => a -> IO ()
printF = putStr . show

count :: (VUM.Unbox t, Num b) => (t -> Bool) -> VU.Vector t -> b
count f = VU.foldr' (\x -> if f x then (+ 1) else id) 0

main :: IO ()
main = do 
    n <- getInt
    print . length . List.nub =<< getListIntLn n