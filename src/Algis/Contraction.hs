{-# LANGUAGE UnicodeSyntax
  , FlexibleInstances
  #-}

module Algis.Contraction where

import Algis.Orbitals
import Algis.Tensor

import qualified Data.Map as M
import Data.List (nub, sort, (\\), subsequences, intercalate)
import Control.Monad (guard)
import Data.Maybe (catMaybes, fromMaybe)

type IndexPartition = [Int]

-- | This is the main type to get information from contractions. For
--   every list we know its result as a tensor
type Contraction o
  = M.Map [o] (Tensor o)

class Contract a where
  contract ∷ Contraction OrbitalIndex → a → [a]

contractWith ∷ IndexPartition → Contraction OrbitalIndex → [o] → [a]
contractWith ints cos os = undefined

contractWithPartition
  ∷ IndexPartition
  → Contraction AlgisGenerator
  → [AlgisGenerator]
  → [AlgisTensor]
contractWithPartition is cos gs
  = mconcat . catMaybes $ go <$> combis
    where
      combis = getSubsequences is gs
      go ∷ [[AlgisGenerator]] → Maybe [AlgisTensor]
      go (c:cs) = (:) <$> M.lookup c cos <*> go cs

getAllCombinations ∷ Ord a ⇒ [a] → [[[a]]]
getAllCombinations as = mconcat [getSubsequences p as | p ← partitions]
  where partitions = possiblePartitions as

getSubsequences ∷ Ord a
                ⇒ IndexPartition     -- ^ Partition (e.g. [2,2,4])
                → [a]       -- ^ List to find partitions in
                → [ [[a]] ] -- ^ Combinations
getSubsequences is xs = nub $ sort <$> getSubsequences' is xs

-- | TODO: write it tail-recursively
getSubsequences' ∷ Eq a ⇒ IndexPartition → [a] → [ [[a]] ]
getSubsequences' (i:is) xs = do
  choice ← sub' i xs
  let c = fst choice
      r = snd choice
  n ← getSubsequences' is r
  pure $ c : n
  where sub' n xs = [(x, xs \\ x) | x ← subsequences xs, length x == n]
getSubsequences' [] _ = [[]]

-- |
-- >>> possiblePartitions "abcd"
-- [[4], [2,2]]
possiblePartitions ∷ [a] → [IndexPartition]
possiblePartitions as
  | odd $ l = []
  | otherwise = do
      evs ← evensList
      c <- cartesianN evs
      guard $ sum c == l
      guard $ isSorted c
      pure c
  where l = length as
        maxNpairs = l `div` 2
        evens = [x | x ← [1..l], even x]
        evensList = [replicate i evens | i <- [1..maxNpairs]]
        isSorted xs = xs == (sort xs)

cartesianN :: [[a]] -> [[a]]
cartesianN as = cartesianN' as [[]]

cartesianN' :: [[a]] -> [[a]] -> [[a]]
cartesianN' [] rest = reverse <$> rest
cartesianN' (a:as) rest = cartesianN' as $ cart' a rest
  where
    cart' ∷ [a] -> [[a]] -> [[a]]
    cart' as bs = [x : y | x <- as, y <- bs]

-- | TODO
instance Contract AlgisTensor where
  contract cos (Tensor n os) = [Tensor n os]
  contract cos t = [t]

instance Contract AlgisExpr where
  contract cos (TSum txs) = mconcat $ contract cos <$> txs
  contract cos (T t) = T <$> contract cos t
  contract cos (TProd ts) = contract cos $ T t
    where
        name = intercalate " " $ tensorName <$> ts
        os = mconcat $ tensorOperators <$> ts
        t = Tensor name os
  contract cos (TScal λ tx) = TScal λ <$> contract cos tx
