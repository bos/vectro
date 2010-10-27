{-# LANGUAGE BangPatterns #-}

module Data.Vectro
    (
      Vect
    , fromList
    , fromVector
    , index
    , isSane
    , snoc
    , update
    , Vector
    , toVector
    , showStructure
    ) where

import Control.DeepSeq
import Data.Bits hiding (shift)
import qualified Data.Vector as V
import qualified Data.Vectro.Vect as T
import Data.Vectro.Vect (Vect(..))

data Vector a = Z !(Vect a) !Int !(V.Vector a)

instance Show a => Show (Vector a) where
    show = show . toList

instance NFData a => NFData (Vector a) where
    rnf (Z t _ c) = rnf t `seq` rnf c `seq` ()

showStructure ::  Show a => Vector a -> String
showStructure (Z t k c) = "Z (" ++ T.showStructure t ++ ") " ++ show k ++
                          " " ++ show (V.toList c)

toZ :: Int -> Vect a -> Vector a
toZ k t = go t
  where go (Leaf v)   = Z t (k .&. complement T.mask) v
        go (Node s v) = go (v V.! ((k `shiftR` s) .&. T.mask))

fromList :: [a] -> Vector a
fromList = toZ 0 . T.fromList

toList :: Show a => Vector a -> [a]
toList = T.toList . fromZ

fromVector :: V.Vector a -> Vector a
fromVector = toZ 0 . T.fromVector

toVector :: Vector a -> V.Vector a
toVector = T.toVector . fromZ

isSane :: Vector a -> Bool
isSane (Z t _ c) = T.isSane t && V.length c <= T.factor

fromZ :: Vector a -> Vect a
fromZ (Z t k c) = go t
  where go (Leaf _)   = Leaf c
        go (Node s v) = Node s (v V.// [(j, go (v V.! j))])
            where j = (k `shiftR` s) .&. T.mask

update :: Show a => Vector a -> Int -> a -> Vector a
update z@(Z t k c) j n
    | jm == k   = Z t k (c V.// [(j-jm,n)])
    | otherwise = toZ j (T.update (fromZ z) j n)
    where jm = j .&. complement T.mask

index :: Vector a -> Int -> a
index (Z t k c) j
    | jm == k   = c V.! (j-jm)
    | otherwise = T.index t j
    where jm = j .&. complement T.mask

snoc :: Vector a -> a -> Vector a
snoc (Z t k c) n | V.length c < T.factor = Z t k (V.snoc c n)
                 | otherwise = Z (T.snocChunk t c) k (V.singleton n)
