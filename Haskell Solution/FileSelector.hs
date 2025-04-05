module FileSelector (selectFile) where

import System.Directory (listDirectory, doesFileExist, getCurrentDirectory)
import System.FilePath ((</>))
import Control.Monad (filterM)
import Text.Read (readMaybe)

-- Select a file from a given subdirectory
selectFile :: FilePath -> IO FilePath
selectFile relativeDir = do
  cwd <- getCurrentDirectory
  let dir = cwd </> relativeDir
  files <- listDirectory dir
  validFiles <- filterM (\f -> doesFileExist (dir </> f)) files

  if null validFiles
    then error "No files found."
    else do
      putStrLn $ "\nFiles in " ++ relativeDir ++ ":"
      mapM_ (\(i, f) -> putStrLn $ "  [" ++ show i ++ "] " ++ f) (zip [1..] validFiles)
      promptUser validFiles dir

promptUser :: [FilePath] -> FilePath -> IO FilePath
promptUser files dir = do
  putStr "Select a file by number: "
  input <- getLine
  case readMaybe input of
    Just n | n >= 1 && n <= length files -> return (dir </> files !! (n - 1))
    _ -> putStrLn "Invalid selection." >> promptUser files dir