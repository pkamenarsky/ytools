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
identifier = many1 $ letter <|> (char '.')

expr :: Parser Expr
expr = parseAnd <|> parseOr <|> liftM Token identifier 

parseAnd :: Parser Expr
parseAnd = liftM2 And expr (string " && " >> expr)

parseOr :: Parser Expr
parseOr = liftM2 And expr (string " || " >> expr)

parseSelect :: Parser (Select String)
parseSelect = do
	string "select "
	field <- identifier
	string " from "
	source <- identifier
	return $ Select field source

main = print $ parse expr "source" "asd && sdfff"

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
