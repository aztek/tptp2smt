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
pretty (Problem units) = concatMap prettyUnit units

comment :: String -> S -> String
comment comment s = "; " ++ comment ++ "\n" ++ prettyS s ++ "\n"

prettyUnit :: Unit -> String
prettyUnit (SortDeclaration un s) = comment un $ C [A "declare-sort", toS s, A "0"]
prettyUnit (SymbolDeclaration un c ss s) = comment un $ C [A "declare-fun", toS c, C (map toS ss), toS s]
prettyUnit (Axiom un f) = comment un $ C [A "assert", toS f]
prettyUnit (Conjecture un f) = comment un $ C [A "assert", toS (Negate f)]

instance ToS Formula where
  toS (Distinct cs) = C (A "distinct" : map toS cs)
  toS (Quantified q vs f) = C [toS q, C (map (\(v,s) -> C [toS v, toS s]) vs), toS f]
  toS (Equality a b) = C [A "=", toS a, toS b]
  toS (Binary c a b) = C [toS c, toS a, toS b]

instance ToS Term where
  toS (Var v) = toS v
  toS (Const c) = toS c

instance ToS Symbol where
  toS (Symbol c) = A c

instance ToS Variable where
  toS (Variable v) = A v

instance ToS Sort where
  toS (Sort s) = A s

instance ToS Quantifier where
  toS Forall = A "forall"
  toS Exists = A "exists"

instance ToS Connective where
  toS Conjunction = A "&"
  toS Disjunction = A "|"
  toS Equivalence = A "="
