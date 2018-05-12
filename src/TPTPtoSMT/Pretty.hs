module TPTPtoSMT.Pretty where

import Data.List

import TPTPtoSMT.Problem

data S
  = A String
  | C [S]

prettyS :: S -> String
prettyS (A a) = a
prettyS (C as) = "(" ++ unwords (map prettyS as) ++ ")"

class ToS a where
  toS :: a -> S

pretty :: Problem -> String
pretty (Problem units) = unlines (map prettyUnit units) ++ "(check-sat)"

prettyUnit :: (UnitName, Unit) -> String
prettyUnit (un, u) = "; " ++ un ++ "\n" ++ prettyS (toS u) ++ "\n"

instance ToS Unit where
  toS (SortDeclaration s) = C [A "declare-sort", toS s, A "0"]
  toS (SymbolDeclaration c ss s) = C [A "declare-fun", toS c, C (map toS ss), toS s]
  toS (Axiom f) = C [A "assert", toS f]
  toS (Conjecture f) = C [A "assert", toS (Negate f)]

instance ToS Formula where
  toS (Distinct cs) = C (A "distinct" : map toS cs)
  toS (Quantified q vs f) = C [toS q, C (map (\(v,s) -> C [toS v, toS s]) vs), toS f]
  toS (Equality a b) = C [A "=", toS a, toS b]
  toS (App f ts) = C $ toS f : map toS ts
  toS (Binary c a b) = C [toS c, toS a, toS b]
  toS (Negate f) = C [A "not", toS f]
  toS (Constant True) = A "true"
  toS (Constant False) = A "false"

instance ToS Term where
  toS (Var v) = toS v
  toS (Const c) = toS c

instance ToS Symbol where
  toS (Symbol c) = A c

instance ToS Variable where
  toS (Variable v) = A v

instance ToS Sort where
  toS (Sort "$o") = A "Bool"
  toS (Sort s) = A s

instance ToS Quantifier where
  toS Forall = A "forall"
  toS Exists = A "exists"

instance ToS Connective where
  toS Conjunction = A "and"
  toS Disjunction = A "or"
  toS Equivalence = A "="
