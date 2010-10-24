import Data.Vectro
import Test.Framework (defaultMain, testGroup)
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2 (testProperty)

ints :: Vect Int -> Vect Int
ints a = a

instance Arbitrary a => Arbitrary (Vect a) where
    arbitrary = fromList `fmap` arbitrary

t_isSane = isSane . ints

t_snoc_sane a = isSane . (`snoc` a) . ints

main = defaultMain tests

tests = [
   testProperty "t_isSane" t_isSane,
   testProperty "t_snoc_sane" t_snoc_sane
 ]
