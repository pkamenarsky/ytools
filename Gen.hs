{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Gen where

import Debug.Trace

import Control.Monad

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.Typeable
import Data.Data
import Data.List

data TypeEnum = IntType | FloatType | BoolType | DateType | StringType deriving (Enum, Show, Eq)
data Typed a = Typed TypeEnum a deriving Show

type KeyPath = [String]

data Schema = Object [(String, Schema)] |
	Field TypeEnum |
	Array Schema deriving Show

data Permissions = Permissions Bool Bool Bool deriving (Data, Typeable, Show)
data FileType = FileType {
	pathname :: String,
	subdirs :: [FileType],
	permissions ::Permissions
} deriving (Data, Typeable, Show) 
	
data FileString = FileString {address :: String, age :: [Int], fi :: FileInt} deriving (Data, Typeable)
data FileInt = FileInt {something::String} deriving (Data, Typeable)

fieldType :: Type -> Q Exp
fieldType (ConT name)
	| name == ''String = [| Field StringType |]
	| name == ''Bool = [| Field BoolType |]
	| name == ''Int = [| Field IntType |]
	| otherwise = listFields name
fieldType (AppT ListT typ) = [| Array $(fieldType typ) |]

listFields :: Name -> Q Exp
listFields name = do
	rf <- reify name
	let fields = case rf of
		TyConI (DataD _ _ _ [RecC _ fields] _) -> fields
		a -> error $ show a
	let names = map (\(name, _, typ) -> typ) fields
	let specs = listE $ map (\(name, _, typ) -> let sn = nameBase name in [| (sn, $(fieldType typ)) |]) fields
	[| Object $specs |]
	--stringE $ show names

