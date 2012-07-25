{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Control.Monad

import Data.Data
import Data.Typeable

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

data TypeEnum = IntType | FloatType | BoolType | DateType | StringType deriving (Enum, Show, Eq)
data Value = forall a. Type a => Value a

data Value2 = IntValue Int | StringValue String deriving (Eq, Ord, Data, Typeable)

class Ord a => Type a where
	valueType :: a -> TypeEnum

instance Type Int where
	valueType _ = IntType

instance Type Bool where
	valueType _ = BoolType

compareTypes :: Value -> Value -> Bool
compareTypes (Value a) (Value b) = valueType a == valueType b

compareValues :: Value -> Value -> Ordering
compareValues (Value a) (Value b) = undefined

produce :: Value
produce = Value (6 :: Int)
