import Control.Monad
import Control.Applicative

import System.Directory
import System.Posix

import System.IO.Unsafe

--

ls :: [FilePath]
ls = unsafePerformIO $ getDirectoryContents "."

ll = map info ls

--

data FileInfo = FileInfo { path :: String, size :: FileOffset }

instance Show FileInfo where
	show f = path f ++ " " ++ show (size f)

info :: FilePath -> FileInfo
info path = unsafePerformIO $ do
	stat <- getFileStatus path
	return $ FileInfo path $ fileSize stat

--

-- (-->) :: (Monad m) => m [a] -> (a -> m b) -> m [b]
-- (-->) = \ls info -> ls >>= mapM info

(-->) :: [a] -> ([a] -> [b]) -> [b]
(-->) a b = b a

main = print $ ls
