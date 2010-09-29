{-# LANGUAGE BangPatterns #-}

module Data.Vectro
    (
      Vect
    , fromList
    , fromVector
    ) where

import Data.Bits hiding (shift)
import qualified Data.Vector as V
import Data.Vector (Vector)

data Vect a = Node !(Vector (Vect a))
            | Leaf !(Vector a)
              deriving (Eq, Ord)

instance Show a => Show (Vect a) where
    show (Leaf v) = show (V.toList v)
    show (Node v) = show (V.toList v)

factor :: Int
factor = 4
{-# INLINE factor #-}

shift :: Int
shift = 2
{-# INLINE shift #-}

mask :: Int
mask = 0x3
{-# INLINE mask #-}

fromVector :: Vector a -> Vect a
fromVector v0
    | len0 < factor = Leaf v0
    | otherwise     = toTree (numChildren len0) (leaves v0 len0)
  where
    len0 = V.length v0
    leaves v !rem | rem >= factor  = let h = V.unsafeTake factor v
                                         t = V.unsafeDrop factor v
                                     in Leaf h : leaves t (rem-factor)
                  | rem == 0       = []
                  | otherwise      = [Leaf v]

fromList :: [a] -> Vect a
fromList xs = case map (Leaf . V.fromList) . chunksOf factor $ xs of
                []  -> Leaf V.empty
                [l] -> l
                ls  -> toTree (length ls) ls

toTree :: Int -> [Vect a] -> Vect a
toTree len ns
    | len <= factor = Node $ V.fromList ns
    | otherwise     = toTree (numChildren len)
                             (map (Node . V.fromList) $ chunksOf factor ns)

chunksOf :: Int -> [a] -> [[a]]
chunksOf k = go
  where go [] = []
        go xs = let (h,t) = splitAt k xs
                in h : go t

numChildren :: Int -> Int
numChildren k | s /= 0    = n + 1
              | otherwise = n
  where n = k `shiftR` shift
        s = k .&. mask
