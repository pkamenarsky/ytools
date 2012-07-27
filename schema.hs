module Schema where

import Numeric

import Control.Applicative
import Control.Monad

import Data.Data
import Data.Typeable

import Text.JSON

data TypeEnum = IntType | FloatType | BoolType | DateType | StringType deriving (Enum, Show, Eq)
data Typed a = Typed TypeEnum a deriving Show

type KeyPath = [String]

data Schema = Object [(String, Schema)] |
	Field TypeEnum |
	Array Schema

lsSchema = Object [
	("path", Field StringType),
	("size", Field IntType),
	("subdirs", Array lsSchema),
	("permissions", Object [
		("oread", Field BoolType)])]

lookupType :: Schema -> KeyPath -> Either String TypeEnum
lookupType _ [] = Left "Empty keypath"
lookupType (Object fields) [x] = case lookup x fields of
	Just (Field t) -> Right t
	Just (Object _) -> Left $ "Key " ++ x ++ " is an object"
	Just (Array _) -> Left $ "Key " ++ x ++ " is an array"
	_ -> Left $ "No field " ++ x ++ " found in object"
lookupType (Object fields) (x:xs) = case lookup x fields of
	Just s@(Object _) -> lookupType s xs
	Just (Field t) -> Left $ "Key " ++ x ++ " is a field but keypath is not exhausted"
	Just (Array _) -> Left $ "Key " ++ x ++ " is an array"
	_ -> Left $ "No field " ++ x ++ " found in object"
lookupType _ _ = Left $ "Invalid keypath"

safeRead :: (Read a) => (String -> [(a, String)]) -> String -> String -> Either String a
safeRead rf msg str = case [x | (x, t) <- rf str, ("", "") <- lex t] of
	[x] -> Right x
	_ -> Left $ str ++ " not a " ++ msg

compareJSValues :: JSValue -> JSValue -> Either String Ordering
compareJSValues = undefined

extractJSValue :: TypeEnum -> KeyPath -> JSValue -> Either String JSValue
extractJSValue = undefined

stringToJSValue :: TypeEnum -> String -> Either String JSValue
stringToJSValue IntType str = JSRational True <$> safeRead readFloat "int" str
stringToJSValue BoolType "true" = Right $ JSBool True
stringToJSValue BoolType "false" = Right $ JSBool False
stringToJSValue BoolType str = JSBool <$> safeRead read "bool" str
stringToJSValue StringType str = Right $ JSString $ toJSString str

