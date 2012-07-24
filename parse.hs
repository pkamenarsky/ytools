{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Applicative hiding ((<|>))
import Control.Monad

import System.Environment

import System.Posix.Files
import System.Posix.IO

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Token

import Text.JSON

-- Units

data Type = IntType | StringType
data Value = IntValue Int | StringValue String deriving (Show, Eq, Ord)

compareValues :: Value -> Value -> Either String Ordering
compareValues (IntValue a) (IntValue b) = Right $ compare a b
compareValues (StringValue a) (StringValue b) = Right $ compare a b
compareValues c1 c2 = Left $ "Type mismatch: (" ++ show c1 ++ ") - (" ++ show c2 ++ ")"

parseValue :: String -> Value
parseValue x = IntValue 5

parseUnit :: String -> Type -> Value
parseUnit s IntType = IntValue $ read s
parseUnit s StringType = StringValue s

exprType :: String -> String -> Bool
exprType s1 s2 = let
	p1 = parseValue s1
	p2 = parseValue s2
	in p1 == p2

-- Expressions

data Operand = Operand Type Value | KeyPath [String]

operand :: Parser Operand
operand = operandKeyPath <|> operandInt <|> operandString

operandKey :: [String] -> Parser [String]
operandKey keys = try $ do
	p <- many1 alphaNum
	(char '.' >> operandKey (p:keys)) <|> (return (p:keys))

operandKeyPath :: Parser Operand
operandKeyPath = KeyPath <$> (operandKey [])

operandInt :: Parser Operand
operandInt = try $ do
	p <- many1 digit
	return $ Operand IntType $ IntValue $ read p

operandString :: Parser Operand
operandString = try $ do
	p <- many1 alphaNum
	return $ Operand StringType $ StringValue p

data Expr = And Expr Expr | Or Expr Expr | Equal String String | Greater String String | Less String String deriving Show

idntf :: Parser String
idntf = try $ many1 $ alphaNum <|> char '.'

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
	e1 <- idntf
	op opstr
	e2 <- idntf
	return $ ctr e1 e2

parseAnd :: Parser (Expr -> Expr -> Expr)
parseAnd = op "&&" >> return And

parseOr :: Parser (Expr -> Expr -> Expr)
parseOr = op "||" >> return Or

eval :: JSValue -> Expr -> Either String Bool
eval (JSObject obj) (Equal e1 e2) = case valFromObj e1 obj of
	Ok val -> case compareValues (parseValue val) (parseValue e2) of
		Right EQ -> Right True
		Right _ -> Right False
		Left error -> Left error
	_ -> Left $ "Value " ++ e1 ++ " could not be extracted"
eval obj (And s1 s2) = (&&) <$> eval obj s1 <*> eval obj s2
eval obj (Or s1 s2) = (||) <$> eval obj s1 <*> eval obj s2

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
