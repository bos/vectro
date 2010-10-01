{-# LANGUAGE ScopedTypeVariables #-}
import Criterion.Main
import Data.Vectro as TV
import Data.Vector as V

main = do
  let l = [1..(1000::Int)]
  let v  = V.fromList l
  let tv = TV.fromVector v
  defaultMain [
        bgroup "build" [
            bench "tv" $ nf TV.fromList l
          , bench "v" $ whnf V.fromList l
          , bench "tvv" $ nf TV.fromVector v
          ],
        bgroup "update" [
            bench "v" $ whnf (V.foldl' (\v i -> v V.// [(i-1,0)]) v) v
          , bench "tv" $ nf (V.foldl' (\t i -> TV.update t (i-1) 0) tv) v
          ]
        ]
