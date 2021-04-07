module TeXtractor where

import           System.Directory               ( doesFileExist )

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
  noLeadSpace s
    |
    -- Commented line
      head s == '%' = Unimportant
    |
    -- Input command
      take 7 s == "\\input{" && drop (length s - 1) s == "}" = Input
    $ drop 7
    $ take (length s - 1) s
    |
    -- Starts with backslash
      head s == '\\' = Unimportant
    |
    -- Starts with $
      head s == '$' = Unimportant
    |
    -- Else, the line is text
      otherwise = Text s


