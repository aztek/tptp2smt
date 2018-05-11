module TPTPtoSMT.Parse where

import Data.List ((\\))
import Data.Char

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

import TPTPtoSMT.Problem

language = emptyDef
  { Token.commentLine     = "#"
  , Token.reservedNames   = [ "tff", "(", ")", "$tType"
                            , "type", "axiom", "conjecture"
                            , "$true", "$false", "$distinct"
                            ]
  , Token.reservedOpNames = [ "&", "|", "=>", "<=>"
                            , "?", "!", ".", "~", "'"
                            ]
  }

lexer = Token.makeTokenParser language

reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
integer    = Token.integer    lexer
semi       = Token.semi       lexer
whiteSpace = Token.whiteSpace lexer
braces     = Token.braces     lexer
brackets   = Token.brackets   lexer
commaSep   = Token.commaSep   lexer
symbol     = Token.symbol     lexer

constant name fun = reserved name >> return fun

quantifier = constant "?" Exists
         <|> constant "!" Forall

connective = constant "&" Conjunction
         <|> constant "|" Disjunction
         <|> constant "<=>" Equivalence

unitName = Token.identifier lexer

upperWord = do
  fc  <- oneOf firstChar
  r   <- optionMaybe (many $ oneOf rest)
  spaces
  return $ case r of
             Nothing -> [fc]
             Just s  -> fc:s
  where firstChar = ['A'..'Z']
        rest      = firstChar ++ ['0'..'9'] ++ ['a'..'z'] ++ "_"

lowerWord = do
  fc  <- oneOf firstChar
  r   <- optionMaybe (many $ oneOf rest)
  spaces
  return $ case r of
             Nothing -> [fc]
             Just s  -> fc:s
  where firstChar = ['a'..'z']
        rest      = firstChar ++ ['0'..'9'] ++ ['A'..'Z'] ++ "_"

singleQuoted = do
  symbol "'"
  sq <- many $ oneOf $ map chr $ [32..38] ++ [40..91] ++ [93..126]
  symbol "'"
  return sq

var = Variable <$> upperWord
symbol' = Symbol <$> (try singleQuoted <|> lowerWord)
sort = Sort <$> (symbol "$o" <|> try singleQuoted <|> lowerWord)

term = Var <$> var
   <|> Const <$> symbol'

distinct = reserved "$distinct" >> Distinct <$> parens (commaSep symbol')

quantified = do
  q <- quantifier
  vs <- brackets . commaSep $ do { v <- var; reserved ":"; s <- sort; return (v, s) }
  f <- formula
  return $ Quantified q vs f

equality = do
  a <- term
  reserved "="
  b <- term
  return $ Equality a b

binary = do
  a <- formula
  c <- connective
  b <- formula
  return $ Binary c a b

formula = distinct
      <|> quantified
      <|> equality
      <|> binary
      <|> (reserved "~" >> Negate <$> formula)

tff p = do
  reserved "tff"
  t <- parens p
  symbol "."
  return t

sortDeclaration = tff $ do
  un <- unitName
  reserved ","
  reserved "type"
  reserved ","
  s <- sort
  reserved ":"
  reserved "$tType"
  return $ SortDeclaration un s

typ = try (do ss <- parens $ sepBy sort $ symbol "*"
              reserved ">"
              s <- sort
              return (ss, s))
  <|> try (do s' <- sort
              reserved ">"
              s <- sort
              return ([s'], s))
  <|> try (do s <- sort
              return ([], s))

symbolDeclaration = tff $ do
  un <- unitName
  reserved ","
  reserved "type"
  reserved ","
  c <- symbol'
  reserved ":"
  (ss, s) <- typ
  return $ SymbolDeclaration un c ss s

axiom = tff $ do
  un <- unitName
  reserved ","
  reserved "axiom"
  reserved ","
  f <- formula
  return $ Axiom un f

conjecture = do
  reserved "tff"
  reserved "("
  un <- unitName
  reserved ","
  reserved "conjecture"
  reserved ","
  f <- formula
  reserved ")"
  reserved "."
  return $ Conjecture un f

unit = try sortDeclaration <|> try symbolDeclaration <|> try axiom <|> try conjecture

parser :: Parser Problem
parser = Problem <$> many unit

parseTPTP :: SourceName -> String -> Either ParseError Problem
parseTPTP = parse parser
