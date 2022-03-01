{-# LANGUAGE UnicodeSyntax
  #-}


import Test.Hspec
import TestCombinatorics

import Algis
import qualified Data.Set as S
import qualified Data.Map as M

mkOSet ∷ String → String → OrbitalSet
mkOSet n ids = OrbitalSet n $ S.fromList . (:[]) $ ids

φ ∷ String → OrbitalSet → OrbitalIndex
φ = OrbitalIndex

α, α' ∷ OrbitalIndex → Generator OrbitalIndex
α = Generator Fermion
α' = Generator' Fermion

ij = [i', j]
i' = α' $ φ "i" holes
j = α $ φ "j" holes

tabij = Tensor "T" [ α' $ φ "a" particles
                   , α' $ φ "b" particles
                   , α  $ φ "j" holes
                   , α  $ φ "i" holes
                   ]

vijab = Tensor "V" [ α' $ φ "i" particles
                   , α' $ φ "j" particles
                   , α  $ φ "b" holes
                   , α  $ φ "a" holes
                   ]

particles = mkOSet "Particles" "abcdefgh"
holes     = mkOSet "Holes"     "ijklmno"

phContractions ∷ Contraction AlgisGenerator
phContractions
 = M.fromList [ ([α' i, α j], De (α' i) (α j))
              , ([α a, α' b], De (α a) (α' b))
              ]
  where i = φ "i" holes
        j = φ "j" holes
        a = φ "a" particles
        b = φ "b" particles

main ∷ IO ()
main = do
  hspec $ do

    testCombinatorics

    describe "Test some contraction" $ do
      it "should recover ij" $ do
        contractWithPartition [2] phContractions (ij)
                `shouldBe` [De i' j]
      it "Should contract Tabij" $ do
        contractWithPartition [2,2] phContractions (tensorOperators tabij)
                `shouldBe` []
        -- contractWithPartition [2,2] phContractions (tensorOperators (tensorProduct vijab tabij))
        --         `shouldBe` []
