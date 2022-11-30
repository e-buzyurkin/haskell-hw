import qualified Data.ByteString as B
import System.Directory

-- task 1 Sanity Check
printFile :: FilePath -> IO ()
printFile fileToPrint = do
   content <- readFile fileToPrint
   putStrLn content

-- task 2 Text Check
areEqualText :: FilePath -> FilePath -> IO (Bool)
areEqualText file1 file2 = do
    content1 <- readFile file1
    content2 <- readFile file2
    return (content1 == content2)

-- task 3 Dos2Unix
dos2unix :: FilePath -> IO ()
dos2unix path = do
    content <- readFile path
    let converted = foldl (\acc c -> if c /= '\r' then (acc ++ [c]) else acc) "" content
    --generates "path_out" filename
    let outPath = foldl (\acc c -> if c == '.' then (acc ++ "_out" ++ [c]) else (acc ++ [c])) "" path
    writeFile outPath converted

-- task 3 Dos2Unix
unix2dos :: String -> IO ()
unix2dos path = do
    content <- readFile path
    let converted = foldl (\acc c -> if c == '\n' then (acc ++ "\r" ++ [c]) else (acc ++ [c])) "" content
    --generates "path_out" filename
    let outPath = foldl (\acc c -> if c == '.' then (acc ++ "_out" ++ [c]) else (acc ++ [c])) "" path
    writeFile outPath converted

-- task 4 Binary check
areEqualBin :: FilePath -> FilePath -> IO (Bool)
areEqualBin file1 file2 = do
    content1 <- B.readFile file1
    content2 <- B.readFile file2 
    return (content1 == content2) :: IO (Bool)

-- task 5 https://imgur.com/a/r8yrxFk
fileIsBeingEdited :: FilePath -> IO (Bool)
fileIsBeingEdited path = doesFileExist (path ++ ".sw")






