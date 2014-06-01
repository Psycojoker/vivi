import Data.ByteString.Lazy(hGet, ByteString(), append, empty)
import Data.ByteString.Lazy.Char8(unpack)
import qualified Data.ByteString.Lazy as BS
import Codec.Compression.GZip(decompress)
import Control.Applicative(pure, (<*>))
import Data.List(isPrefixOf, isSuffixOf)
import Data.Maybe(listToMaybe, fromMaybe)
import Data.Text(pack)
import Data.Text.IO(hPutStr)
import System.Environment(getArgs)
import System.Directory(getDirectoryContents)
import System.IO(hClose, Handle)
import System.IO.Temp(openTempFile)
import System.Process(createProcess, proc, std_out, std_err, cwd, StdStream(CreatePipe), waitForProcess)

matchingLogs :: Maybe String -> [String] -> [String]
matchingLogs Nothing = filter $ isPrefixOf ("access.")
matchingLogs (Just name) = filter $ isPrefixOf (name ++ "_access.")

rawFilesLogs :: [String] -> IO String
rawFilesLogs files = pure (++) <*> (readFilesLog readFile ".log" files) <*> (readFilesLog readFile ".log.1" files)

gzipedFilesLogs :: [String] -> IO String
gzipedFilesLogs = readFilesLog (fmap (unpack . decompress) . BS.readFile) ".gz"

readFilesLog :: (FilePath -> IO String) -> String -> ([String] -> IO String)
readFilesLog f extension = (fmap concat . sequence) . map f . filter (isSuffixOf extension)

fullPathLs :: FilePath -> Maybe String -> IO [String]
fullPathLs path name = getDirectoryContents path >>= return . map (path ++) . matchingLogs name

stream :: Handle -> IO ByteString
stream h = do
    content <- hGet h (1024*1024)
    if content == empty
       then return empty
       else pure append <*> (return content) <*> stream h

getLogs :: Maybe String -> IO [String]
getLogs name = do
    logs <- fullPathLs "/var/log/apache2/" name
    if length logs == 0
       then fullPathLs "/var/log/nginx/" name
       else return logs

main = do
    name <- getArgs >>= return . listToMaybe
    apacheLogs <- getLogs name
    logsContent <- pure (++) <*> (rawFilesLogs apacheLogs) <*> (gzipedFilesLogs apacheLogs)
    (tempFilePath, tempFileHandle) <- openTempFile "/tmp" (fromMaybe "for_visitors" name)
    hPutStr tempFileHandle $ pack logsContent
    hClose tempFileHandle

    (_, Just hout, Just herr, pid) <- createProcess (proc "visitors" [tempFilePath]){ cwd = Just "/tmp", std_out = CreatePipe, std_err = CreatePipe }

    stream hout >>= BS.writeFile ((fromMaybe "stats" name) ++ ".html")
