{-# LANGUAGE UnicodeSyntax #-}

module Algis.Orbitals where

import qualified Data.Set as S
import Data.Tree (Tree)

-- | We need to represent a set where some orbital information
--   is implicitily stored. For instance
--   \[
--       \mathrm{Particles} = \{a, b, c\}
--   \]
--   would mean
--   @OrbitalSet "Particles" (Set.fromList "abc")@
data OrbitalSet = OrbitalSet String (S.Set String)
  deriving (Show, Eq, Ord)

-- | An orbital index need a name and a context.
--   The context is in which set this orbital is given.
data OrbitalIndex = OrbitalIndex String OrbitalSet
  deriving (Show, Eq, Ord)

-- | We have to map arbitrary partition of indices.  We can do this
--   using a tree
--
--   The root nodes are the primitive indices, and branch nodes are
--   sets of indices which are a direct sum of the index sets below
--   them.
type OrbitalTree = Tree OrbitalSet
