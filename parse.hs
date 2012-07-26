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

import Schema

-- Expressions

data LValue = LKeyPath KeyPath | Command String KeyPath deriving Show
data RValue = RString String | RKeyPath KeyPath deriving Show

-- We could have just defined Expr l v instead of going through Term but then we wouldn't be able
-- to make a functor out of it.
data Term l v = Term l v deriving show
data Expr t = And (Expr t) (Expr t) | Or (Expr t) (Expr t) | Equal t | Greater t | Less t deriving Show

instance Functor Expr where
	fmap f (Equal t) = Equal $ f t
	fmap f (Greater t) = Greater $ f t
	fmap f (Less t) = Less $ f t
	fmap f (And l r) = And (fmap f l) (fmap f r)
	fmap f (Or l r) = Or (fmap f l) (fmap f r)

-- Parsing

lexer = T.makeTokenParser emptyDef {
	identLetter = char '.' <|> alphaNum,
	reservedOpNames = ["&&", "||", "=", "==", "<", ">"]
}

lexeme = T.lexeme lexer
identifier = T.identifier lexer
reservedOp = T.reservedOp lexer
parens = T.parens lexer

lvalue :: Parser LValue
lvalue = LKeyPath <$> (lexeme $ key []) where
	key :: [String] -> Parser [String]
	key keys = try $ do
		p <- many1 letter
		(char '.' >> key (keys ++ [p])) <|> (return (keys ++ [p]))

rvalue :: Parser RValue
rvalue = RString <$> lexeme (many1 (oneOf ":.,-" <|> alphaNum)) <|>
	RString <$> lexeme (between (char '\'') (char '\'') (many1 $ satisfy (/= '\'')))

parseEq :: (Term LValue RValue -> Expr t) -> String -> Parser (Expr t)
parseEq ctr opstr = ctr <$> (Term <$> lvalue <* reservedOp opstr <*> rvalue)

parseOp :: String -> a -> Parser a
parseOp str ctr = reservedOp str >> return ctr

expr :: Parser (Expr (Term LValue RValue))
expr = chainl1 (parens expr <|>
	(try (parseEq Equal "==") <|>
		parseEq Equal "=") <|>
	parseEq Greater ">" <|>
	parseEq Less "<")
	(parseOp "&&" And <|> parseOp "||" Or)

-- Evaluation

-- extractValue :: JSValue -> LValue -> Either String String
-- extractValue (JSObject obj) (LKeyPath [key]) = case valFromObj key obj of
	-- Ok v -> Right v
	-- _ -> Left $ "Extracting value failed: " ++ key
-- extractValue (JSObject obj) (LKeyPath (key:keys)) = case valFromObj key obj of
	-- Ok obj' -> extractValue obj' (LKeyPath keys)
	-- _ -> Left $ "Extracting value failed: " ++ key
-- 
-- eval :: JSValue -> Expr -> (String -> Ordering) -> Either String Bool
-- eval obj (Equal e1 e2) cmpFn = case cmpFn <$> extractValue obj e1 of
	-- Right ord -> Right $ EQ == ord
	-- Left e -> Left e
-- eval obj (And s1 s2) cmpFn = (&&) <$> eval obj s1 cmpFn <*> eval obj s2 cmpFn
-- eval obj (Or s1 s2) cmpFn = (||) <$> eval obj s1 cmpFn <*> eval obj s2 cmpFn
-- 
-- makeEvalFn :: Schema -> Expr -> (JSValue -> Either String Bool)
-- makeEvalFn schema (Equal e1 e2) = let f = cmpFn schema e1 e2 in
	-- \value -> case f <$> extractValue value e1 of
		-- Right ord -> (==) <$> ord <*> (return EQ)
		-- Left e -> Left e
-- makeEvalFn schema (And e1 e2) = let
	-- f1 = makeEvalFn schema e1
	-- f2 = makeEvalFn schema e2 in
		-- \value -> (&&) <$> f1 value <*> f2 value

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
	let obj = makeObj
		[("path", JSString $ toJSString "cool"),
		("size", JSString $ toJSString "456"),
		("permissions", makeObj
			[("read", JSString $ toJSString "true")])]
	case parse ((,) <$> expr <*> getInput) "" "path = 'cool' && size = 456 && permissions.read = 'true'" of
		Right (pexp, "") -> print pexp
		Right (_, rest) -> error $ "Unconsumed input: " ++ rest
		Left e -> error $ show e
	print 5
