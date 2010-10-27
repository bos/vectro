import Data.Vectro.Vect as T
import Data.Vectro as V
import Test.Framework (defaultMain, testGroup)
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2 (testProperty)

vints :: Vector Int -> Vector Int
vints a = a

tints :: Vect Int -> Vect Int
tints a = a

instance Arbitrary a => Arbitrary (Vect a) where
    arbitrary = T.fromList `fmap` arbitrary

instance Arbitrary a => Arbitrary (Vector a) where
    arbitrary = V.fromList `fmap` arbitrary

t_isSane = T.isSane . tints
v_isSane = V.isSane . vints

t_snoc_sane a = T.isSane . (`T.snoc` a) . tints
v_snoc_sane a = V.isSane . (`V.snoc` a) . vints

main = defaultMain tests

tests = [
   testProperty "t_isSane" t_isSane,
   testProperty "t_snoc_sane" t_snoc_sane,
   testProperty "v_isSane" v_isSane,
   testProperty "v_snoc_sane" v_snoc_sane
 ]
