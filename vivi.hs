import Data.ByteString.Lazy.Char8(unpack)
import qualified Data.ByteString.Lazy as BS
import Codec.Compression.GZip(decompress)
import Data.List(isPrefixOf, isSuffixOf)
import Data.Maybe(listToMaybe, fromMaybe)
import Data.Text(pack)
import Data.Text.IO(hGetContents, hPutStr)
import System.Environment(getArgs)
import System.Directory(getDirectoryContents)
import System.IO(hClose)
import System.IO.Temp(openTempFile)
import System.Process(createProcess, proc, std_out, cwd, StdStream(CreatePipe), waitForProcess)

matchingLogs :: Maybe String -> [String] -> [String]
matchingLogs Nothing = filter $ isPrefixOf ("access.")
matchingLogs (Just name) = filter $ isPrefixOf (name ++ "_access.")

concatIOStringList :: [IO String] -> IO String
concatIOStringList = fmap concat . sequence

rawFilesLogs :: [String] -> IO String
rawFilesLogs files = concatIOStringList [readFilesLog readFile ".log" files, readFilesLog readFile ".log.1" files]

gzipedFilesLogs :: [String] -> IO String
gzipedFilesLogs = readFilesLog (fmap (unpack . decompress) . BS.readFile) ".gz"

readFilesLog :: (FilePath -> IO String) -> String -> ([String] -> IO String)
readFilesLog f extension = concatIOStringList . map f . filter (isSuffixOf extension)

fullPathLs :: FilePath -> Maybe String -> IO [String]
fullPathLs path name = getDirectoryContents path >>= return . map (path ++) . matchingLogs name

main = do
    name <- getArgs >>= return . listToMaybe
    apacheLogs <- fullPathLs "/var/log/apache2/" name
    rawLogsContent <- rawFilesLogs apacheLogs
    gzipedLogsContent <- gzipedFilesLogs apacheLogs
    (tempFilePath, tempFileHandle) <- openTempFile "/tmp" (fromMaybe "for_visitors" name)
    hPutStr tempFileHandle $ pack $ rawLogsContent ++ gzipedLogsContent
    hClose tempFileHandle

    (_, Just hout, _, pid) <- createProcess (proc "visitors" [tempFilePath]){ cwd = Just "/tmp", std_out = CreatePipe }
    hGetContents hout >>= putStrLn . show
    waitForProcess pid
    putStrLn tempFilePath
