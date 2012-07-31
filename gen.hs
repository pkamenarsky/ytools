{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

import Data.Typeable
import Data.Data

data Permissions = Permissions Bool Bool Bool deriving (Data, Typeable, Show)
data FileType = FileType {
	pathname :: String,
	subdirs :: [FileType],
	permissions ::Permissions
} deriving (Data, Typeable, Show) 

ft = FileType "asd" [] (Permissions False True True)

f :: Data a => a -> String
f d = (concat $ gmapQ f d) ++ (show $ dataTypeOf d)

main = do
	let	dt = dataTypeOf (undefined :: FileType)
		ctr = head $ dataTypeConstrs dt
		ctrType = constrType ctr
		fields = constrFields ctr
	print (cast ft :: Maybe FileType)
	print $ gmapQ f ft
