module Main (main) where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L -- (1)
import Test.QuickCheck.Text (number)


data Expr
  = Var String
  | IntConst Integer
  | BoolConst Bool
  | StringConst String
  deriving (Show)

data Stmt
  = Seq [Stmt]
  | Assign String Expr
  | If Bool Stmt Stmt
  | Print Expr
  | Skip
  deriving (Show)

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

rword :: Text -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

rws :: [String] -- list of reserved words
rws = ["if","then","else","skip","true","false","print"]



identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

integer :: Parser Integer
integer = lexeme L.decimal

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

bool :: Parser Bool
bool = (True <$ rword "true") <|> (False <$ rword "false")



stmt :: Parser Stmt
stmt = ifStmt <|> printStmt <|> skipStmt <|> assignStmt

stmtSeq :: Parser Stmt
stmtSeq = f <$> sepBy1 stmt ";"
  -- if there's only one stmt return it without using ‘Seq’
  where f l = if length l == 1 then head l else Seq l



exprParse :: Parser Expr
exprParse = StringConst <$> stringLiteral
  <|> IntConst <$> integer
  <|> BoolConst <$> bool
  <|> Var <$> identifier

ifStmt :: Parser Stmt
ifStmt = do
  rword "if"
  cond  <- bool
  rword "then"
  stmt1 <- stmt
  rword "else"
  stmt2 <- stmt
  return (If cond stmt1 stmt2)

assignStmt :: Parser Stmt
assignStmt = do
  var  <- identifier
  _ <- symbol ":="
  expr <- exprParse
  return (Assign var expr)

skipStmt :: Parser Stmt
skipStmt = Skip <$ rword "skip"

printStmt :: Parser Stmt
printStmt = do
  rword "print"
  expr <- exprParse
  return (Print expr)

main :: IO ()
main = do
  let x = "if false then y := 3 else print true;skip;print \"hello\""
  parseTest stmtSeq x