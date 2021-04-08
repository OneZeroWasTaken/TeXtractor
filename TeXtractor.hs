module TeXtractor where

import           System.Directory               ( doesFileExist )
import           Data.List                      ( isSuffixOf )

data LineType = Unimportant | Text String | Input FilePath | SkipSection String


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
  SkipSection s -> extract (skipSection s ls) acc

-- | Parses a .tex file line into a LineType
parseLine :: String -> LineType
parseLine [] = Unimportant
parseLine l  = noLeadSpace $ dropWhile (== ' ') l
 where
  noLeadSpace s
    |
    -- Commented line
      head s == '%'                    = Unimportant
    |
    -- Input command
      isInputOrInclude s               = Input $ getFileName s
    |
    -- Begin document should not be skipped
      take 16 s == "\\begin{document}" = Unimportant
    |
    -- Ignore what is inside other \begin sections
      take 7 s == "\\begin{"           = SkipSection $ getStringInsideBrackets s
    |
    -- Starts with backslash
      head s == '\\'                   = Unimportant
    |
    -- Starts with $
      head s == '$'                    = Unimportant
    |
    -- Else, the line is text
      otherwise                        = Text s


-- | Will remove elements from the list until it finds its corresponding \end
skipSection :: String -> [String] -> [String]
skipSection s [] = error $ "Parse error: Missing \\end line for " ++ s
skipSection s (l : ls) | getStringInsideBrackets l == s = ls
                       | otherwise                      = skipSection s ls

-- | Finds out if it is an input or include statement
isInputOrInclude :: String -> Bool
isInputOrInclude s | take 7 s == "\\input{" && lastChar s == "}" = True
                   | take 9 s == "\\include{" && lastChar s == "}" = True
                   | otherwise = False
  where lastChar s' = drop (length s' - 1) s'

-- | Given an input or include line, returns the name of 
-- | the file with the added .tex if it was omitted
getFileName :: String -> FilePath
getFileName s | ".tex" `isSuffixOf` name = name
              | otherwise                = name ++ ".tex"
  where name = getStringInsideBrackets s

-- | Extracts the string that is inside the curly brackets of the given string
-- | If no brackets exist, returns the original string
getStringInsideBrackets :: String -> String
getStringInsideBrackets s
  | length firstPartRemoved > 1 = lastPartRemoved $ tail firstPartRemoved
  | otherwise                   = s
 where
  --lastCharRemoved  = take (length firstPartRemoved - 1) firstPartRemoved
  firstPartRemoved = dropWhile (/= '{') s
  lastPartRemoved  = takeWhile (/= '}')


