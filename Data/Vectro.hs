{-# LANGUAGE BangPatterns #-}

module Data.Vectro
    (
      Vect
    , fromList
    , toList
    , fromVector
    , toVector
    , index
    , isSane
    , snoc
    , update
    , Vector
    , showStructure
    ) where

import Debug.Trace
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
toZ k t
    | k == l && l .&. T.mask == 0 = Z t k V.empty
    | otherwise       = go t
  where go (Leaf v)   = Z t (k .&. notMask) v
        go (Node s v) = go (v V.! ((k `shiftR` s) .&. T.mask))
        l = T.length t

fromList :: [a] -> Vector a
fromList = toZ 0 . T.fromList

toList :: Show a => Vector a -> [a]
toList = T.toList . fromZ

fromVector :: V.Vector a -> Vector a
fromVector = toZ 0 . T.fromVector

toVector :: Vector a -> V.Vector a
toVector = T.toVector . fromZ

isSane :: Vector a -> Bool
isSane (Z t k c) = T.isSane t && k <= T.length t && V.length c <= T.factor

fromZ :: Vector a -> Vect a
fromZ (Z t k c)
    | k < (l .&. notMask) - 1 = go t
    | l > 0 && l .&. T.mask == 0 = T.snocChunk t c
    | otherwise = fixLast t c
  where go (Leaf _)   = Leaf c
        go (Node s v) = Node s (v V.// [(j, go (v V.! j))])
            where j = (k `shiftR` s) .&. T.mask
        l = T.length t

update :: Vector a -> Int -> a -> Vector a
update z@(Z t k c) j n
    | jm == k   = Z t k (c V.// [(j-jm,n)])
    | otherwise = toZ j (T.update (fromZ z) j n)
    where jm = j .&. notMask

index :: Vector a -> Int -> a
index (Z t k c) j
    | jm == k   = c V.! (j-jm)
    | otherwise = T.index t j
    where jm = j .&. notMask

snoc :: Show a => Vector a -> a -> Vector a
snoc z@(Z t k c) n
  | k < (l.&.notMask) - 1 = snoc (toZ (T.length t') t') n
  | V.length c < T.factor = Z t k (V.snoc c n)
  | l > 0 && l.&.T.mask == 0 = Z (T.snocChunk t c) (k+T.factor) (V.singleton n)
  | otherwise             = Z (fixLast t c) (k+T.factor) (V.singleton n)
  where t' = fromZ z
        l = T.length t

fixLast t c = go t
    where go (Leaf _)   = Leaf c
          go (Node s v) = Node s (V.init v `V.snoc` go (V.last v))

notMask :: Int
notMask = complement T.mask
