import Control.Applicative hiding ((<|>))
import Control.Monad

import System.Environment

import System.Posix.Files
import System.Posix.IO

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Char

data Select a = Select String String deriving Show

data Expr = And Expr Expr | Or Expr Expr | Token String deriving Show

identifier :: Parser String
identifier = try $ many1 $ letter <|> char '.'

op :: String -> Parser String
op str = try $ do
	spaces
	s <- string str
	spaces
	return s

expr :: Parser Expr
expr = chainl1 parseToken (parseAnd <|> parseOr)

parseToken :: Parser Expr
parseToken = Token <$> identifier

parseAnd :: Parser (Expr -> Expr -> Expr)
parseAnd = op "&&" >> return And

parseOr :: Parser (Expr -> Expr -> Expr)
parseOr = op "||" >> return Or

parseSelect :: Parser (Select String)
parseSelect = do
	string "select "
	field <- identifier
	string " from "
	source <- identifier
	return $ Select field source

main = print $ parse expr "source" "aaas && tokenA && tokenB || tokenC && tokenD"

parseArgs :: [String] -> IO ()
parseArgs ["-R"] = print "YES"
parseArgs ["-U"] = print "NO"
parseArgs [] = print "NOTHING"

main2 = do
	getArgs >>= parseArgs
	status <- getFdStatus stdOutput
	if isCharacterDevice status
		then print "char"
		else print "notchar"
