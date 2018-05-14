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
pretty (Problem units) = unlines
                       $ ["(set-logic UF)", ""] ++ 
                         map prettyUnit units ++
                         ["(check-sat)"]

prettyUnit :: (UnitName, Unit) -> String
prettyUnit (un, u) = "; " ++ un ++ "\n" ++ prettyS (toS u) ++ "\n"

instance ToS Unit where
  toS (SortDeclaration s) = C [A "declare-sort", toS s, A "0"]
  toS (SymbolDeclaration c [] s) = C [A "declare-const", toS c, toS s]
  toS (SymbolDeclaration c ss s) = C [A "declare-fun", toS c, C (map toS ss), toS s]
  toS (Axiom f) = C [A "assert", toS f]
  toS (Conjecture f) = C [A "assert", toS (Negate f)]

unorderedPairs :: [a] -> [(a, a)]
unorderedPairs [] = []
unorderedPairs (a:as) = [(a, b) | b <- as] ++ unorderedPairs as

instance ToS Formula where
  -- toS (Distinct cs) = C (A "distinct" : map toS cs)
  toS (Distinct cs) = C $ A "and" : ineqs
    where
      ineqs = [C[A "not", C [A "=", toS a, toS b]] | (a, b) <- unorderedPairs cs]
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

toSymbol :: String -> String
toSymbol s | all (`elem` allowedChars) s = s
           | otherwise = "\\" ++ s ++ "\\"
  where
    allowedChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "+-/*=%?!.$_˜&ˆ<>@"

instance ToS Symbol where
  toS (Symbol c) = A (toSymbol c)

instance ToS Variable where
  toS (Variable v) = A (toSymbol v)

instance ToS Sort where
  toS (Sort "$o") = A "Bool"
  toS (Sort s) = A (toSymbol s)

instance ToS Quantifier where
  toS Forall = A "forall"
  toS Exists = A "exists"

instance ToS Connective where
  toS Conjunction = A "and"
  toS Disjunction = A "or"
  toS Equivalence = A "="
