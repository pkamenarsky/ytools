module Schema where

import Control.Applicative
import Control.Monad

import Data.Data
import Data.Typeable

import Text.JSON

data TypeEnum = IntType | FloatType | BoolType | DateType | StringType deriving (Enum, Show, Eq)
data Typed a = Typed TypeEnum a

data LValue = LKeyPath KeyPath deriving (Eq, Show)
data RValue = RString String | RKeyPath KeyPath deriving (Eq, Show)

type KeyPath = [String]

data Schema = Object [(String, Schema)] |
	Field TypeEnum |
	Array Schema

lsSchema = Object [
	("path", Field StringType),
	("subdirs", Array lsSchema),
	("permissions", Object [
		("oread", Field BoolType)])]

lookupKP :: Schema -> KeyPath -> Either String TypeEnum
lookupKP _ [] = Left "Empty keypath"
lookupKP (Object fields) [x] = case lookup x fields of
	Just (Field t) -> Right t
	Just (Object _) -> Left $ "Key " ++ x ++ " is an object"
	Just (Array _) -> Left $ "Key " ++ x ++ " is an array"
	_ -> Left $ "No field " ++ x ++ " found in object"
lookupKP (Object fields) (x:xs) = case lookup x fields of
	Just s@(Object _) -> lookupKP s xs
	Just (Field t) -> Left $ "Key " ++ x ++ " is a field but keypath is not exhausted"
	Just (Array _) -> Left $ "Key " ++ x ++ " is an array"
	_ -> Left $ "No field " ++ x ++ " found in object"
lookupKP _ _ = Left $ "Invalid keypath"

-- Would be better if cmpFn took an LValue and an RValue and produced a function that
-- extracts the LValue from a js object and then compares it to the RValue (could be a keypath
-- in later versions), i.e: cmpFn :: LValue -> RValue -> (JSObject -> Ordering)
-- cmpFn is going to be useful for ysort, ygroup etc too.

safeRead :: Read a => String -> String -> Either String a
safeRead str msg = case reads str of
	[] -> Left $ str ++ " not an " ++ msg
	(_, y:ys):xs -> Left $ str ++ " not an " ++ msg
	[(a, [])] -> Right a

cmpFn :: Typed LValue -> Typed RValue -> Either String (JSValue -> Either String Ordering)
--cmpFn (Typed IntType _) (Typed _ (RString rstr)) = case safeRead rstr "integer" :: (Either String Int) of
	--Right rstr' -> Right $ \lstr -> compare <$> safeRead lstr "integer" <*> return rstr'
	--Left e -> Left e
cmpFn (Typed IntType _) (Typed IntType (RString rstr)) = do
	r <- safeRead rstr "integer" :: Either String Int
	return cmp where
		cmp (JSRational _ r') = Right $ compare r r'
		cmp _ = Left $ "Type mismatch"
	-- return $ \lstr -> compare <$> safeRead lstr "integer" <*> return rstr'


-- cmpFn StringType s = \s' -> Right $ compare s s'
-- cmpFn IntType s = let i = read s :: Int in p where
-- 		p (JSInteger i') = compare i i'
-- 		p _ = error "Type mismatch"
-- cmpFn IntType s = let i = read s :: Int in \s' -> Right $ compare i $ read s'
-- cmpFn FloatType s = let f = read s :: Float in \s' -> Right $ compare f $ read s'
-- cmpFn t s = error $ "Type mismatch: " ++ s ++ " is not a " ++ show t

