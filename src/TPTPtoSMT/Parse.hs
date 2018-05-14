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
  , Token.reservedNames   = [ "tff", "$tType"
                            , "type", "axiom", "conjecture"
                            , "$true", "$false", "$distinct"
                            , ".", "?", "!", ":"
                            ]
  , Token.reservedOpNames = [ "&", "|", "<=>", "~", "=" ]
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

boolConst = constant "$true"  (Constant True)
        <|> constant "$false" (Constant False)

unitName = Token.identifier lexer

upperWord = do
  fc <- oneOf firstChar
  r  <- optionMaybe (many $ oneOf rest)
  spaces
  return $ case r of
             Nothing -> [fc]
             Just s  -> fc:s
  where firstChar = ['A'..'Z']
        rest      = firstChar ++ ['0'..'9'] ++ ['a'..'z'] ++ "_$"

lowerWord = do
  fc  <- oneOf firstChar
  r   <- optionMaybe (many $ oneOf rest)
  spaces
  return $ case r of
             Nothing -> [fc]
             Just s  -> fc:s
  where firstChar = ['a'..'z']
        rest      = firstChar ++ ['0'..'9'] ++ ['A'..'Z'] ++ "_$"

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

connectives = [
  [ Infix  (reservedOp "|"   >> return (Binary Disjunction)) AssocLeft ],
  [ Infix  (reservedOp "&"   >> return (Binary Conjunction)) AssocLeft ],
  [ Infix  (reservedOp "<=>" >> return (Binary Equivalence)) AssocNone ],
  [ Prefix (reservedOp "~"   >> return Negate) ]
  ]

formula :: Parser Formula
formula = buildExpressionParser connectives arg

atom = App <$> symbol' <*> parens (commaSep term)

distinct = reserved "$distinct" >> Distinct <$> parens (commaSep symbol')

quantified = do
  q <- quantifier
  vs <- brackets . commaSep $ do
    v <- var
    reserved ":"
    s <- sort
    return (v, s)
  reserved ":"
  f <- formula
  return $ Quantified q vs f

equality = do
  a <- term
  reservedOp "="
  b <- term
  return $ Equality a b

arg = parens formula
  <|> distinct
  <|> quantified
  <|> try equality
  <|> atom
  <|> (reservedOp "~" >> Negate <$> formula)
  <|> boolConst

sortDeclaration = do
  s <- sort
  reserved ":"
  reserved "$tType"
  return $ SortDeclaration s

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

symbolDeclaration = do
  c <- symbol'
  reserved ":"
  (ss, s) <- typ
  return $ SymbolDeclaration c ss s

typedDeclaration = try sortDeclaration <|> symbolDeclaration

unit = (reserved "type" >> symbol "," >> typedDeclaration)
   <|> (reserved "axiom" >> symbol "," >> Axiom <$> formula)
   <|> (reserved "conjecture" >> symbol "," >> Conjecture <$> formula)

namedUnit = do
  reserved "tff"
  (un, u) <- parens $ do
    un <- unitName
    symbol ","
    u <- unit
    return (un, u)
  reserved "."
  return (un, u)

parser :: Parser Problem
parser = Problem <$> many namedUnit

parseTPTP :: SourceName -> String -> Either ParseError Problem
parseTPTP = parse parser
