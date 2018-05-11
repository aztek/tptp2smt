module TPTPtoSMT.Problem where

newtype Symbol = Symbol String
newtype Variable = Variable String
newtype Sort = Sort String
type UnitName = String

data Term
  = Var Variable
  | Const Symbol

data Quantifier
  = Forall
  | Exists

data Connective
  = Conjunction
  | Disjunction
  | Equivalence

data Formula
  = Distinct [Symbol]
  | Quantified Quantifier [(Variable, Sort)] Formula
  | Equality Term Term
  | App Symbol [Term]
  | Binary Connective Formula Formula
  | Negate Formula

data Unit
  = SortDeclaration UnitName Sort
  | SymbolDeclaration UnitName Symbol [Sort] Sort
  | Axiom UnitName Formula
  | Conjecture UnitName Formula

newtype Problem = Problem [Unit]
