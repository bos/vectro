{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}

module Data.Vectro.Vect
    (
      Vect(..)
    , empty
    , length
    , fromList
    , toList
    , fromVector
    , toVector
    , index
    , isSane
    , snoc
    , update
    , factor
    , mask
    , snocChunk
    , showStructure
    ) where

import Control.DeepSeq (NFData(rnf))
import Data.Bits hiding (shift)
import qualified Data.List as L
import qualified Data.Vector as V
import Prelude hiding (length)

data Vect a = Node !Int !(V.Vector (Vect a))
            | Leaf !(V.Vector a)
              deriving (Eq, Ord)

showStructure :: Show a => Vect a -> String
showStructure (Leaf l)   = show $ V.toList l
showStructure (Node s v) = "Node " ++ show s ++ " (" ++ tidy ++ ")"
    where tidy = concat . L.intersperse "," . map showStructure . V.toList $ v

instance NFData a => NFData (V.Vector a) where
    rnf = V.foldl' (\_ v -> rnf v) ()

instance NFData a => NFData (Vect a) where
    rnf (Leaf v)   = rnf v
    rnf (Node _ v) = rnf v

instance Show a => Show (Vect a) where
    show = show . toList

factor :: Int
factor = 4
{-# INLINE factor #-}

shift :: Int
shift = 2
{-# INLINE shift #-}

mask :: Int
mask = 0x3
{-# INLINE mask #-}

fromVector :: V.Vector a -> Vect a
fromVector v0
    | len0 <= factor = Leaf v0
    | otherwise      = toTree shift (numChildren len0) (leaves v0 len0)
  where
    len0 = V.length v0
    leaves v !r | r >= factor  = let h = V.unsafeTake factor v
                                     t = V.unsafeDrop factor v
                                 in Leaf h : leaves t (r-factor)
                | r == 0       = []
                | otherwise    = [Leaf v]

fromList :: [a] -> Vect a
fromList xs = case map (Leaf . V.fromList) . chunksOf factor $ xs of
                []  -> Leaf V.empty
                [l] -> l
                ls  -> toTree shift (L.length ls) ls

toTree :: Int -> Int -> [Vect a] -> Vect a
toTree !h len ns
    | len <= factor = Node h $ V.fromList ns
    | otherwise     = toTree h' (numChildren len)
                             (map (Node h . V.fromList) $ chunksOf factor ns)
    where h' = h+shift

toList :: Vect a -> [a]
toList (Leaf l)   = V.toList l
toList (Node _ v) = concatMap toList . V.toList $ v

toVector :: Vect a -> V.Vector a
toVector (Leaf l)   = l
toVector (Node _ v) = V.concatMap toVector v

empty :: Vect a
empty = Leaf V.empty

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

index :: Vect a -> Int -> a
index t k = go t
  where go (Leaf v)   = v V.! (k .&. mask)
        go (Node s v) = go (v V.! ((k `shiftR` s) .&. mask))

update :: Vect a -> Int -> a -> Vect a
update t k n = go t
  where go (Leaf v)   = Leaf (v V.// [(k .&. mask, n)])
        go (Node s v) = Node s (v V.// [(i, go (v V.! i))])
          where !i    = (k `shiftR` s) .&. mask

shiftOf :: Vect a -> Int
shiftOf (Leaf _)   = 0
shiftOf (Node s _) = s

isFull :: Vect a -> Bool
isFull (Leaf v)   = V.length v == factor
isFull (Node s v) = V.length v == factor && V.all isFull v &&
                    V.all ((==s-shift) . shiftOf) v

isSane :: Vect a -> Bool
isSane (Leaf v)   = V.length v <= factor
isSane (Node s v) = V.length v <= factor &&
                    V.all isFull (V.init v) &&
                    isSane (V.last v) &&
                    ((==s-shift) . shiftOf) (V.last v)


snoc :: Vect a -> a -> Vect a
snoc t n = case go t of
             Left n'  -> n'
             Right n' -> Node (shift+shiftOf n') (V.fromList [t,n'])
  where
    go (Leaf v)
      | V.length v < factor = Left $! Leaf (v `V.snoc` n)
      | otherwise           = Right $! Leaf (V.singleton n)
    go (Node s v)
        = case go (V.last v) of
            Left n'                 -> Left $! Node s (V.init v `V.snoc` n')
            Right n'
              | V.length v < factor -> Left $! Node s (v `V.snoc` n')
              | otherwise           -> Right $! Node s (V.singleton n')

snocChunk :: Vect a -> V.Vector a -> Vect a
snocChunk t c = case go t of
                  Left n' -> n'
                  Right n' -> Node (shift+shiftOf n') (V.fromList [t,n'])
  where
    go (Leaf _)    = Right l
    go (Node s v)
        = case go (V.last v) of
            Left n' -> Left $! Node s (V.init v `V.snoc` n')
            Right n'
                | V.length v < factor -> Left $! Node s (v `V.snoc` n')
                | otherwise           -> Right $! Node s (V.singleton n')
    l = Leaf c

length :: Vect a -> Int
length (Leaf l) = V.length l
length (Node _ v) = V.foldl' add 0 v
    where add n c = n + length c

mapVect :: (a -> b) -> Vect a -> Vect b
mapVect f = go
  where
    go (Node s v) = Node s (V.map go v)
    go (Leaf v)   = Leaf (V.map f v)

instance Functor Vect where
    fmap = mapVect
