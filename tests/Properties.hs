{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Vectro.Vect as T
import qualified Data.Vectro as V
import qualified Data.Vector as Vector
import Data.List (foldl')
import Test.Framework (defaultMain, testGroup)
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2 (testProperty)

type L = [Int]
type VV = Vector.Vector Int
type T = T.Vect Int
type V = V.Vector Int

vints :: V.Vector Int -> V.Vector Int
vints a = a

tints :: T.Vect Int -> T.Vect Int
tints a = a

instance Arbitrary a => Arbitrary (T.Vect a) where
    arbitrary = T.fromList `fmap` arbitrary

instance Arbitrary a => Arbitrary (V.Vector a) where
    arbitrary = V.fromList `fmap` arbitrary

instance Arbitrary a => Arbitrary (Vector.Vector a) where
    arbitrary = Vector.fromList `fmap` arbitrary

t_isSane = T.isSane . tints
v_isSane = V.isSane . vints

t_list_id l = (T.toList . T.fromList $ l) == (l::L)
v_list_id l = (V.toList . V.fromList $ l) == (l::L)
t_vector_id v = (T.toVector . T.fromVector $ v) == (v::VV)
v_vector_id v = (V.toVector . V.fromVector $ v) == (v::VV)

snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]

t_snoc (l::L) x = T.toList (T.snoc (T.fromList l) x) == snoc l x
v_snoc (l::L) x = V.toList (V.snoc (V.fromList l) x) == snoc l x

t_snoc_sane a = T.isSane . (`T.snoc` a) . tints
v_snoc_sane a = V.isSane . (`V.snoc` a) . vints

t_snocs (l::L) xs = T.toList (foldl' T.snoc (T.fromList l) xs) ==
                    foldl' snoc l xs
v_snocs (l::L) xs = V.toList (foldl' V.snoc (V.fromList l) xs) ==
                    foldl' snoc l xs

t_snocs_sane (t::T) = T.isSane . foldl' T.snoc t
v_snocs_sane (v::V) = V.isSane . foldl' V.snoc v

main = defaultMain tests

tests = [
   testProperty "t_isSane" t_isSane,
   testProperty "v_isSane" v_isSane,
   testProperty "t_list_id" t_list_id,
   testProperty "v_list_id" v_list_id,
   testProperty "t_vector_id" t_vector_id,
   testProperty "v_vector_id" v_vector_id,
   testProperty "t_snoc" t_snoc,
   testProperty "v_snoc" v_snoc,
   testProperty "t_snoc_sane" t_snoc_sane,
   testProperty "v_snoc_sane" v_snoc_sane,
   testProperty "t_snocs" t_snocs,
   testProperty "v_snocs" v_snocs,
   testProperty "t_snocs_sane" t_snocs_sane,
   testProperty "v_snocs_sane" v_snocs_sane
 ]
