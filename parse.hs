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

data Type = IntType | StringType deriving Show
data Value = IntValue Int | StringValue String deriving (Show, Eq, Ord)

compareValues :: Value -> Value -> Either String Ordering
compareValues (IntValue a) (IntValue b) = Right $ compare a b
compareValues (StringValue a) (StringValue b) = Right $ compare a b
compareValues c1 c2 = Left $ "Type mismatch: (" ++ show c1 ++ ") - (" ++ show c2 ++ ")"

-- Parsing

data Operand = Operand Type Value | KeyPath [String] deriving Show
data Expr = And Expr Expr | Or Expr Expr | Equal Operand Operand | Greater Operand Operand | Less Operand Operand deriving Show

-- Operands

operandKey :: [String] -> Parser [String]
operandKey keys = try $ do
	p <- many1 letter
	(char '.' >> operandKey (keys ++ [p])) <|> (return (keys ++ [p]))

operandKeyPath :: Parser Operand
operandKeyPath = try $ KeyPath <$> (operandKey [])

operandInt :: Parser Operand
operandInt = try $ do
	p <- many1 digit
	return $ Operand IntType $ IntValue $ read p

operandString :: Parser Operand
operandString = try $ do
	p <- many1 alphaNum
	return $ Operand StringType $ StringValue p

operandQuotedString :: Parser Operand
operandQuotedString = try $ do
	char '\''
	p <- many1 alphaNum
	char '\''
	return $ Operand StringType $ StringValue p

operandValue :: Parser Operand
operandValue = operandInt <|> operandString

operand :: Parser Operand
operand = operandKeyPath <|> operandInt <|> operandQuotedString

-- Operators

op :: String -> Parser String
op str = try $ do
	spaces
	s <- string str
	spaces
	return s

-- Expressions

parseEq :: (Operand -> Operand -> Expr) -> String -> Parser Expr
parseEq ctr opstr = try $ do
	e1 <- operand
	op opstr
	e2 <- operand
	return $ ctr e1 e2

parseAnd :: Parser (Expr -> Expr -> Expr)
parseAnd = op "&&" >> return And

parseOr :: Parser (Expr -> Expr -> Expr)
parseOr = op "||" >> return Or

expr :: Parser Expr
expr = chainl1 (parseEq Equal "=" <|> parseEq Greater ">" <|> parseEq Less "<") (parseAnd <|> parseOr)

-- Evaluation

extractValue :: JSValue -> Operand -> Either String Value
extractValue (JSObject obj) (KeyPath [key]) = case valFromObj key obj of
	Ok v -> case parse operandValue "" v of
		Right (Operand t v) -> Right v
		Left e -> Left $ show e
	_ -> Left $ "Extracting value failed: " ++ key
extractValue (JSObject obj) (KeyPath (key:keys)) = case valFromObj key obj of
	Ok obj' -> extractValue obj' $ KeyPath keys
	_ -> Left $ "Extracting value failed: " ++ key
extractValue _ (Operand t v) = Right v

eval :: JSValue -> Expr -> Either String Bool
eval value (Equal e1 e2) = case (compareValues <$> extractValue value e1 <*> extractValue value e2) of
	Right ord -> liftM2 (==) ord $ Right EQ
	Left e -> Left e
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
	let parsed = parse expr "" "path = 'cool' && size = 456 && permissions.read = 'true'"
	case parsed of
		Right e -> putStrLn $ show $ eval (makeObj
			[("path", JSString $ toJSString "cool"),
			("size", JSString $ toJSString "456"),
			("permissions", makeObj
				[("read", JSString $ toJSString "true")])]) e
		Left e -> putStrLn $ "Parse error: " ++ show e
