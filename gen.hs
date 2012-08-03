{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

import Data.Typeable
import Data.Data

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

gen :: Typeable a => a -> Schema
gen a = case typeRepArgs $ typeOf a of
	[x, y] -> field x where
		field t
			| t == typeOf (undefined :: String) = Field StringType
			| t == typeOf (undefined :: Int) = Field IntType
			| otherwise = error "sry :("
	a -> Object $ map (("",) . gen) a
	
data FileString = FileString String deriving (Data, Typeable)
data FileInt = FileInt Int deriving (Data, Typeable)


main = print $ gen FileInt
