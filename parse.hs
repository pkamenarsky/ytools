import Control.Applicative hiding ((<|>))
import Control.Monad

import System.Environment

import System.Posix.Files
import System.Posix.IO

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Char

import Text.JSON

-- Expressions

data Expr = And Expr Expr | Or Expr Expr | Equal String String | Greater String String | Less String String deriving Show

identifier :: Parser String
identifier = try $ many1 $ alphaNum <|> char '.'

op :: String -> Parser String
op str = try $ do
	spaces
	s <- string str
	spaces
	return s

expr :: Parser Expr
expr = chainl1 (parseEq Equal "=" <|> parseEq Greater ">" <|> parseEq Less "<") (parseAnd <|> parseOr)

parseEq :: (String -> String -> Expr) -> String -> Parser Expr
parseEq ctr opstr = try $ do
	e1 <- identifier
	op opstr
	e2 <- identifier
	return $ ctr e1 e2

parseAnd :: Parser (Expr -> Expr -> Expr)
parseAnd = op "&&" >> return And

parseOr :: Parser (Expr -> Expr -> Expr)
parseOr = op "||" >> return Or

eval :: JSValue -> Expr -> Bool
eval (JSObject obj) (Equal e1 e2) = case valFromObj e1 obj of
	Ok val -> val == e2
	_ -> False
eval obj (And s1 s2) = eval obj s1 && eval obj s2
eval obj (Or s1 s2) = eval obj s1 || eval obj s2

-- main

parseArgs :: [String] -> IO ()
parseArgs ["-R"] = putStrLn ""
parseArgs ["-U"] = putStrLn ""
parseArgs [] = putStrLn ""

main = do
	getArgs >>= parseArgs
	status <- getFdStatus stdOutput
	if isCharacterDevice status
		then putStrLn ""
		else putStrLn ""
	let parsed = parse expr "source" "path = cool && size = 456"
	case parsed of
		Right e -> putStrLn $ show $ eval (makeObj
			[("path", JSString $ toJSString "cool"),
			("size", JSString $ toJSString "456")]) e
		Left e -> putStrLn $ "Parse error: " ++ show e
