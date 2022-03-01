{-# LANGUAGE UnicodeSyntax
  #-}
-- | This module defines necessary structures to build up symbolic tensors

module Algis.Tensor where

import Algis.Orbitals

data Particle = Fermion | Boson
  deriving (Show, Eq)

-- | First of all we should define Generators.
--   A generator is an object that generates the Lie Algebra
--   associated with the Fock space, i.e. \(\hat{a}^\dagger\) and so
--   on. Of course we might have a fermionic or a bosonic generator.
--   And a generator should have kind @* → *@ since there is a different
--   generator for every orbital.
data Generator orb
  = Generator Particle orb  -- ^ \(\hat{a}\)
  | Generator' Particle orb -- ^ \(\hat{a}^\dagger\)
  deriving (Show, Eq)

instance Ord orb ⇒ Ord (Generator orb) where
  compare (Generator _ a) (Generator _ b) = compare a b
  compare (Generator' _ a) (Generator _ b) = compare a b
  compare (Generator _ a) (Generator' _ b) = compare a b
  compare (Generator' _ a) (Generator' _ b) = compare a b

-- | We define a tensor as an object with a name
data Tensor o
  = Tensor
    String -- ^ Name of the tensor
    [o]    -- ^ Generators
  | Id     -- ^ Identity tensor
  | Ze     -- ^ Zero tensor
  | De o o -- ^ \(\delta_{pq}\)
  deriving (Eq, Show)

tensorProduct ∷ Tensor o → Tensor o → Tensor o
tensorProduct (Tensor n os) (Tensor n' os')
  = let nn = n <> "*" <> n'
        ooss = os <> os'
    in Tensor nn ooss

tensorName ∷ Show o ⇒ Tensor o → String
tensorName (Tensor n _) = n
tensorName (De o o') = "δ{" ++ show o ++ ", " ++ show o' ++ "}"
tensorName t = show t

tensorOperators ∷ Tensor o → [o]
tensorOperators (Tensor _ os) = os
tensorOperators _ = []

-- | Common arithmetic expressions for tensors
data TExpr λ o
  = T (Tensor o)
  -- ^ Wrap a single value of tensor type
  | TProd [Tensor o]
  -- ^ Product of tensors
  | TSum [TExpr λ o]
  -- ^ Sum of tensors
  | TScal λ (TExpr λ o)
  -- ^ Scalar times a tensor
  deriving (Show, Eq)

-- | This is the main expression that Algis is designed to work with.
type AlgisGenerator = Generator OrbitalIndex
type AlgisExpr = TExpr Float AlgisGenerator
type AlgisTensor = Tensor AlgisGenerator

-- | Class to implement part of a \(C^*\)-algebra.
class Cstar a where
  dag, (†) ∷ a → a
  (†) = dag

instance Cstar (Generator o) where
  dag (Generator fb o)  = Generator' fb o
  dag (Generator' fb o) = Generator fb o

instance (Cstar o) ⇒ Cstar (Tensor o) where
  dag (Tensor n os) = Tensor n $ dag <$> reverse os
  dag t = t

instance Cstar Float where
  dag = id

instance (Cstar λ, Cstar o) ⇒ Cstar (TExpr λ o) where
  dag (T t) = T $ dag t
  dag (TSum ts) = TSum $ dag <$> ts
  dag (TProd ts) = TProd $ dag <$> reverse ts
  dag (TScal λ ts) = TScal (dag λ) (dag ts)
