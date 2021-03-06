import Control.Applicative hiding ((<|>))
import Control.Monad

import qualified Data.Foldable as F

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

data Expr l r = And (Expr l r) (Expr l r) | Or (Expr l r) (Expr l r) | Equal l r | Greater l r | Less l r deriving Show

mapExpr :: (Expr l r -> Either String (Expr l' r')) -> Expr l r -> Either String (Expr l' r')
mapExpr f (And l r) = And <$> mapExpr f l <*> mapExpr f r
mapExpr f (Or l r) = Or <$> mapExpr f l <*> mapExpr f r
mapExpr f t = f t

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

parseEq :: (LValue -> RValue -> Expr LValue RValue) -> String -> Parser (Expr LValue RValue)
parseEq ctr opstr = ctr <$> lvalue <* reservedOp opstr <*> rvalue

parseOp :: String -> a -> Parser a
parseOp str ctr = reservedOp str >> return ctr

expr :: Parser (Expr LValue RValue)
expr = chainl1 (parens expr <|>
	(try (parseEq Equal "==") <|>
		parseEq Equal "=") <|>
	parseEq Greater ">" <|>
	parseEq Less "<")
	(parseOp "&&" And <|> parseOp "||" Or)

-- Typing

typifyExpr :: (Typed LValue -> JSValue -> a) -> LValue -> RValue -> Schema -> Either String a
typifyExpr ctr l@(LKeyPath kp) r@(RString str) schema = case lookupType schema kp of
	Right t -> ctr (Typed t l) <$> stringToJSValue t str
	Left e -> Left e
typifyExpr ctr _ _ schema = error "Unsupported lvalue or rvalue"

typify :: Schema -> Expr LValue RValue -> Either String (Expr (Typed LValue) JSValue)
typify schema = mapExpr convert where
	convert (Equal l r) = typifyExpr Equal l r schema
	convert (Less l r) = typifyExpr Less l r schema
	convert (Greater l r) = typifyExpr Greater l r schema

-- Evaluation

cmp :: Ordering -> KeyPath -> JSValue -> JSValue -> Either String Bool
cmp ord kp jsvalue value = (== ord) <$> (extractJSValue kp value >>= compareJSValues jsvalue)

eval :: (Expr (Typed LValue) JSValue) -> JSValue -> Either String Bool
eval (And l r) value = (&&) <$> eval l value <*> eval r value
eval (Or l r) value = (||) <$> eval l value <*> eval r value
eval (Equal (Typed t (LKeyPath kp)) jsvalue) value = cmp EQ kp jsvalue value
eval (Greater (Typed t (LKeyPath kp)) jsvalue) value = cmp GT kp jsvalue value
eval (Less (Typed t (LKeyPath kp)) jsvalue) value = cmp LT kp jsvalue value
eval _ _ = Left "Unsupported expression"

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
		("size", JSRational True 456),
		("permissions", makeObj
			[("oread", JSBool True)])]
	case parse ((,) <$> expr <*> getInput) "" "path = 'cool' && size = 456 && permissions.oread = 'true'" of
		Right (pexp, "") -> print $ do
			texp <- typify lsSchema pexp
			eval texp obj
		Right (_, rest) -> error $ "Unconsumed input: " ++ rest
		Left e -> error $ show e
	print 5
