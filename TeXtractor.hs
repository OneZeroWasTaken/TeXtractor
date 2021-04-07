module TeXtractor where

import           System.Directory               ( doesFileExist )
import           Data.List                      ( isSuffixOf )

data LineType = Unimportant | Text String | Input FilePath


-- The path to the main .tex file
inputFilePath :: FilePath
inputFilePath = "main.tex"

-- The path to where the output will end up
outputFilePath :: FilePath
outputFilePath = "output.txt"


-- | Run this to extract the input file into an output
main :: IO ()
main = do
  s <- extractFile inputFilePath
  writeFile outputFilePath (unlines . reverse $ s)
  putStrLn $ "Successfully extracted to file " ++ outputFilePath
  --putStrLn $ unlines . reverse $ s

extractFile :: FilePath -> IO [String]
extractFile f = do
  exists <- doesFileExist f
  if exists
    then do
      file <- readFile f
      let allLines = lines file
      extract allLines []
    else do
      putStrLn $ "Could not find file: " ++ f
      return []

extract :: [String] -> [String] -> IO [String]
extract []       acc = return acc
extract (l : ls) acc = case parseLine l of
  Unimportant -> extract ls acc
  Text  s     -> extract ls (s : acc)
  Input f     -> do
    input <- extractFile f
    extract ls (input ++ "" : acc)

-- | Parses a .tex file line into a LineType
parseLine :: String -> LineType
parseLine [] = Unimportant
parseLine l  = noLeadSpace $ dropWhile (== ' ') l
 where
  noLeadSpace s |
    -- Commented line
                  head s == '%'      = Unimportant
                |
    -- Input command
                  isInputOrInclude s = Input $ getFileName s
                |
    -- Starts with backslash
                  head s == '\\'     = Unimportant
                |
    -- Starts with $
                  head s == '$'      = Unimportant
                |
    -- Else, the line is text
                  otherwise          = Text s

isInputOrInclude :: String -> Bool
isInputOrInclude s | take 7 s == "\\input{" && lastChar s == "}" = True
                   | take 9 s == "\\include{" && lastChar s == "}" = True
                   | otherwise = False
  where lastChar s' = drop (length s' - 1) s'

getFileName :: String -> FilePath
getFileName s | ".tex" `isSuffixOf` lastCharRemoved = lastCharRemoved
              | otherwise                           = lastCharRemoved ++ ".tex"
 where
  lastCharRemoved  = take (length firstPartRemoved - 1) firstPartRemoved
  firstPartRemoved = tail $ dropWhile (/= '{') s

