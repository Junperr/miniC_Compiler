module Main (main) where

import System.Environment (getArgs)

import Data.Text (Text, pack)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L -- (1)
import Test.QuickCheck.Text (number)

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Aeson (Value(Bool))

data Expr
  = Var String
  | IntConst Integer
  | BoolConst Bool
  | StringConst String
  deriving (Show)

data Stmt
  = Seq [Stmt]
  | Assign String Expr
  | If Expr Stmt Stmt
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
stmtSeq = do
  stmts <- sepEndBy1 (stmt <* sc) (symbol ";")  -- Parse statements separated by semicolons and consume space after each statement
  return $ if length stmts == 1 then head stmts else Seq stmts




exprParse :: Parser Expr
exprParse = StringConst <$> stringLiteral
  <|> IntConst <$> integer
  <|> BoolConst <$> bool
  <|> Var <$> identifier

ifStmt :: Parser Stmt
ifStmt = do
  rword "if"
  cond  <- exprParse
  rword "then"
  _ <- symbol "{"
  stmt1 <- stmtSeq
  _ <- symbol "}"
  elseBranch <- optional $ do
    rword "else"
    _ <- symbol "{"
    stmt2 <- stmtSeq
    _ <- symbol "}"
    return stmt2
  return $ case elseBranch of
    Just stmt2 -> If cond stmt1 stmt2
    Nothing    -> If cond stmt1 Skip

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


type Env = Map String Expr

interpret :: Env -> Stmt -> IO Env
interpret env stmt = case stmt of
  Skip -> return env  -- Do nothing
  
  Assign var expr -> 
    -- Evaluate the expression and update the environment
    case evalExpr env expr of
      Left err   -> error $ "Evaluation error: " ++ err
      Right val  -> return $ Map.insert var val env
  
  Print expr -> 
    -- Evaluate the expression and print its value
    case evalExpr env expr of
      Left err   -> error $ "Evaluation error: " ++ err
      Right val  -> do 
        putStrLn (show val)
        return env
  
  If cond stmt1 stmt2 -> 
    -- Evaluate the condition and choose the appropriate branch
    case evalExpr env cond of
      Left err    -> error $ "Condition evaluation error: " ++ err
      Right (BoolConst True)  -> interpret env stmt1
      Right (BoolConst False) -> interpret env stmt2
      _ -> error "Condition did not evaluate to a boolean"
  
  Seq stmts -> 
    -- Execute a sequence of statements
    foldl interpretAndUpdate (return env) stmts

interpretAndUpdate :: IO Env -> Stmt -> IO Env
interpretAndUpdate ioEnv stmt = do
  env <- ioEnv         -- Unwrap the current environment from IO
  interpret env stmt    -- Interpret the current statement and return the updated environment


evalExpr :: Env -> Expr -> Either String Expr
evalExpr env expr = case expr of
  Var name -> 
    case Map.lookup name env of
      Just val -> Right val
      Nothing  -> Left $ "Undefined variable: " ++ name

  IntConst i    -> Right (IntConst i)
  BoolConst b   -> Right (BoolConst b)
  StringConst s -> Right (StringConst s)



main :: IO ()
main = do
  let input = "if true then {print \"no\";} else {print \"yes\";};print \"done\""
  case parse stmtSeq "" (Data.Text.pack input) of
    Left err -> putStrLn $ "Parse error: " ++ errorBundlePretty err
    Right parsedStmt -> do
      putStrLn "Program parsed successfully!"
      print parsedStmt
      _ <- interpret Map.empty parsedStmt
      return ()
