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

iterateMaybe :: (t -> Maybe t) -> t -> [t]
iterateMaybe f a = a : case f a of
    Just x -> iterateMaybe f x
    Nothing -> []

iterateMaybeV :: VU.Unbox a => (a -> Maybe a) -> a -> VU.Vector a
iterateMaybeV f a = VU.fromList $ iterateMaybe f a

main :: IO ()
main = do
    _ <- getLine
    v <- getVector
    let x = VU.foldr1 gcd v
    print . (+ (-1)) . VU.length $ iterateMaybeV (\y ->
        case mod y 2 of
            0 -> Just $ y `div` 2
            1 -> Nothing
        )
        x