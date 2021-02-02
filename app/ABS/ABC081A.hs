module Main where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Generic as VG
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Maybe as Maybe
import qualified Data.Array.Repa as R

readInt :: BS8.ByteString -> Int
readInt = fst . Maybe.fromJust . BS8.readInt
getInt :: IO Int
getInt = readInt <$> BS8.getLine

readVector :: BS8.ByteString -> VU.Vector Int
readVector = VU.fromList . map readInt . BS8.split ' '
getVector :: IO (VU.Vector Int)
getVector = readVector <$> BS8.getLine

getMatrix :: Int -> Int -> IO (R.Array R.U R.DIM2 Int)
getMatrix m n = R.fromUnboxed (R.Z R.:. m R.:. n) . VU.concat <$> 
    (replicate m <$> getVector)

printF :: (Show a) => a -> IO ()
printF = putStr . show

main :: IO ()
main = print . BS8.count '1' =<< BS8.getLine