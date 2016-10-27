module TAParser (
  parseExpr
) where

import CalcSyntax

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Data.Functor.Identity

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser emptyDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

infixOp :: String -> (a -> a) -> Ex.Operator String () Identity a
infixOp s f = Ex.Prefix (reservedOp s >> return f)

table :: Ex.OperatorTable String () Identity Expr
table = [[
          infixOp "succ" Succ
        , infixOp "pred" Pred
        , infixOp "iszero" IsZero
        ]]

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reservedOp "then"
  tr <- expr
  reserved "else"
  fl <- expr
  return (If cond tr fl)

true, false, zero :: Parser Expr
true = reserved "true" >> return Tr
false = reserved "false" >> return Fl
zero = reservedOp "0" >> return Zero

factor :: Parser Expr
factor = true
      <|> false
      <|> zero
      <|> ifthen
      <|> parens expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"
