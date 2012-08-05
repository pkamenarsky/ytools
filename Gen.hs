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

ft = FileType "asd" [] (Permissions False True True)

f :: Data a => a -> String
f d = (concat $ gmapQ f d) ++ (show $ dataTypeOf d)

main2 = do
	let	dt = dataTypeOf (undefined :: FileType)
		tt = typeOf FileType
		ctr = head $ dataTypeConstrs dt
		ctrType = constrType ctr
		fields = constrFields ctr
	-- print (cast ft :: Maybe FileType)
	-- print $ gmapQ f ft
	-- print $ constrType ctr
	-- print $ typeRepTyCon tt
	-- print $ typeRepArgs tt
	let AlgRep [c] = dataTypeRep ctrType
	print $ typeRepArgs tt
	print $ typeRepArgs $ head $ tail $ typeRepArgs tt
	print $ typeOf FileString == typeOf (undefined :: String -> FileString)

stype = typeOf (undefined :: String)
itype = typeOf (undefined :: Int)

unfoldType :: Typeable a => a -> [TypeRep]
unfoldType a = unfoldr (\t -> case t of
	[] -> Nothing
	[x, xs] -> Just (x, typeRepArgs xs)) (typeRepArgs $ typeOf a)

-- gen :: Typeable a => a -> Schema
gen a = map (\t -> case t of
	x -> field x where
		field t
			| t == typeOf (undefined :: String) = Field StringType
			| t == typeOf (undefined :: Int) = Field IntType
			| otherwise = error "sry :(") (unfoldType a)
	
data FileString = FileString {address :: String} deriving (Data, Typeable)
data FileInt = FileInt Int String Int deriving (Data, Typeable)

listFields :: Name -> Q Exp
listFields name = do
	rf <- reify name
	let fields = case rf of
		TyConI (DataD _ _ _ [RecC _ fields] _) -> fields
		a -> error $ show a
	let names = map (\(name,_,_) -> name) fields
	stringE $ show names
