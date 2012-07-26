module Schema where

import Control.Monad

import Data.Data
import Data.Typeable

data TypeEnum = IntType | FloatType | BoolType | DateType | StringType deriving (Enum, Show, Eq)

data LValue = LValue KeyPath TypeEnum
data RValue = RString String | RKeyPath KeyPath TypeEnum

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

-- Schemaless cmpFn?

cmpFn :: TypeEnum -> String -> (String -> Ordering) -- (JSValue -> Ordering)
cmpFn StringType s = \s' -> compare s s'
-- cmpFn IntType s = let i = read s :: Int in p where
-- 		p (JSInteger i') = compare i i'
-- 		p _ = error "Type mismatch"
cmpFn IntType s = let i = read s :: Int in \s' -> compare i $ read s'
cmpFn FloatType s = let f = read s :: Float in \s' -> compare f $ read s'
cmpFn t s = error $ "Type mismatch: " ++ s ++ " is not a " ++ show t

