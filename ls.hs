module Ls where

import qualified Control.Exception as E
import Control.Applicative
import Control.Monad

import System.Posix.Directory
import System.Posix.Files

import Data.List

class PrintableC a where
	printC :: a -> String

--

data FileInfo = FileInfo { path :: FilePath, status :: FileStatus }

instance PrintableC FileStatus where
	printC status = show $ fileOwner status

instance PrintableC FileInfo where
	printC info = path info ++ " " ++ printC (status info)

getFileInfo :: FilePath -> IO FileInfo
getFileInfo path = FileInfo <$> return path <*> getFileStatus path

withDirStream = E.bracket (openDirStream ".") closeDirStream

main = catch 
	(withDirStream (\stream -> forever $ do
		info <- readDirStream stream >>= getFileInfo
		putStrLn $ printC info))
	(\e -> return ())
