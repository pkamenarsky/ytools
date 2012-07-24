module Ls where

import qualified Control.Exception as E
import Control.Applicative
import Control.Monad

import System.Posix.Directory
import System.Posix.Files

import Data.Generics
import Data.List

import qualified Text.JSON as J

--

toString = J.JSString . J.toJSString

withDirStream = E.bracket (openDirStream ".") closeDirStream

catchAll :: E.SomeException -> IO ()
catchAll = \e -> return ()

--

class PrintableC a where
	printC :: a -> String

--

data FileInfo = FileInfo { path :: FilePath, status :: FileStatus }

instance PrintableC FileStatus where
	printC status = show $ fileOwner status

instance PrintableC FileInfo where
	printC info = path info ++ " " ++ printC (status info)

instance J.JSON FileInfo where
	readJSON json = undefined
	showJSON info = let
		mode = fileMode (status info)
		checkMode = J.JSBool . (/= nullFileMode) . intersectFileModes mode
		in J.makeObj
			[("path", toString $ path info),
			("permissions", J.makeObj
				[("uwrite", checkMode ownerWriteMode),
				("uread", checkMode ownerReadMode),
				("uexec", checkMode ownerExecuteMode),
				("gwrite", checkMode groupWriteMode),
				("gread", checkMode groupReadMode),
				("gexec", checkMode groupExecuteMode),
				("owrite", checkMode otherWriteMode),
				("oread", checkMode otherReadMode),
				("oexec", checkMode otherExecuteMode)])]

getFileInfo :: FilePath -> IO FileInfo
getFileInfo path = FileInfo <$> return path <*> getFileStatus path

main = E.handle catchAll $ withDirStream $ \stream -> forever $ do
	info <- readDirStream stream >>= getFileInfo
	putStrLn $ J.encodeStrict $ J.showJSON info
