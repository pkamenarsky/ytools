{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Applicative hiding ((<|>))
import Control.Monad

import System.Environment

import System.Posix.Files
import System.Posix.IO

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator()
import Text.ParserCombinators.Parsec.Char()
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as T

import Text.JSON

-- Units

data Value = IntValue Integer | FloatValue Double | StringValue String deriving (Show, Eq, Ord)

compareValues :: Value -> Value -> Either String Ordering
compareValues (IntValue a) (IntValue b) = Right $ compare a b
compareValues (StringValue a) (StringValue b) = Right $ compare a b
compareValues c1 c2 = Left $ "Type mismatch: (" ++ show c1 ++ ") - (" ++ show c2 ++ ")"

-- Parsing

data Operand = Operand Value | KeyPath [String] deriving Show
data Expr = And Expr Expr | Or Expr Expr | Equal Operand Operand | Greater Operand Operand | Less Operand Operand deriving Show

-- Operands

lexer = T.makeTokenParser emptyDef {
	identLetter = char '.' <|> alphaNum,
	reservedOpNames = ["&&", "||", "=", "==", "<", ">"]
}

lexeme = T.lexeme lexer
identifier = T.identifier lexer
naturalOrFloat = T.naturalOrFloat lexer
reservedOp = T.reservedOp lexer
parens = T.parens lexer

nof :: Parser Value
nof = do
	e <- naturalOrFloat
	return $ either IntValue FloatValue e

objValue :: Parser Value
objValue = try
	(nof <* eof) <|>
	StringValue <$> (many1 anyChar <* eof)

exprValue :: Parser Operand
exprValue =
	KeyPath <$> (lexeme $ key []) <|>
	Operand <$> nof <|>
	(Operand . StringValue) <$> lexeme (between (char '\'') (char '\'') (many1 $ satisfy (/= '\'')))
	where
		key :: [String] -> Parser [String]
		key keys = try $ do
			p <- many1 letter
			(char '.' >> key (keys ++ [p])) <|> (return (keys ++ [p]))

-- Expressions

parseEq :: (Operand -> Operand -> Expr) -> String -> Parser Expr
parseEq ctr opstr = ctr <$> exprValue <* reservedOp opstr <*> exprValue

parseOp :: String -> a -> Parser a
parseOp str ctr = reservedOp str >> return ctr

expr :: Parser Expr
expr = chainl1 (parens expr <|>
	(try (parseEq Equal "==") <|>
		parseEq Equal "=") <|>
	parseEq Greater ">" <|>
	parseEq Less "<")
	(parseOp "&&" And <|> parseOp "||" Or)

-- Evaluation

extractValue :: JSValue -> Operand -> Either String Value
extractValue (JSObject obj) (KeyPath [key]) = case valFromObj key obj of
	Ok v -> case parse objValue "" v of
		Right v' -> Right v'
		Left e -> Left $ show e
	_ -> Left $ "Extracting value failed: " ++ key
extractValue (JSObject obj) (KeyPath (key:keys)) = case valFromObj key obj of
	Ok obj' -> extractValue obj' $ KeyPath keys
	_ -> Left $ "Extracting value failed: " ++ key
extractValue _ (Operand v) = Right v

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

main :: IO ()
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
