import System.Environment
import System.Directory
import Control.Monad
import Data.ByteString (readFile)
import Crypto.Hash
import Prelude hiding (readFile)

main :: IO ()
main = do
    args <- getArgs
    files <- filterM doesFileExist args
    hashes <- mapM (\file ->  liftM (hashWith SHA256) (readFile file)) files
    mapM (\(file, hash) -> putStrLn ("File: " ++ file ++ " Hash: " ++ show hash)) $ zip files hashes
    return ()
