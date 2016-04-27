module Predictor where

import qualified Data.Map as M
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Tuple as T
-- import qualified System.Environment as ENV

type Dictionary = M.Map String Int

-- | Specify the characters to keep
keepChars :: Char -> Bool
keepChars c = c `elem` (['A'..'Z'] ++ ['a'..'z'] ++ " \n")

replaceChars :: Char -> Char -> Char -> Char
replaceChars remv repl val
    | val == remv = repl
    | otherwise   = val

-- | Remove characters that should be removed
removeBadChars :: String -> String
removeBadChars str = map (replaceChars '\n' ' ') $ filter keepChars str

-- | Extends toLower to work on a whole string
strToLower :: String -> String
strToLower = map C.toLower

-- | Converts a atring to lower case and removes unwanted characters
fixString :: String -> String
fixString = strToLower . removeBadChars

addWord :: String -> Dictionary -> Dictionary
addWord str = M.insertWith (+) str 1

countWords :: String -> Dictionary
countWords str = foldr addWord M.empty $ words $ fixString str

isMatch :: String -> (String, Int) -> Bool
isMatch str (poss, _) = fixString str `L.isPrefixOf` poss

sortPairs :: [(String, Int)] -> [(String, Int)]
sortPairs pairs = map T.swap $ L.sortBy (flip compare) $ map T.swap pairs

getMatches :: String -> Dictionary -> [(String, Int)]
getMatches "" _      = []
getMatches prev dict = sortPairs $ filter (isMatch prev) $ M.toList dict

getMatchedWords :: String -> Dictionary -> [String]
getMatchedWords "" _      = []
getMatchedWords prev dict = map fst $ getMatches prev dict

initialize :: String -> Dictionary
initialize = countWords 

-- main :: IO ()
-- main = do
--     print "starting"
--     args <- ENV.getArgs
--     text <-  readFile $ head args

--     let dict = countWords text
--     -- print dict
--     print $ sortPairs $ getMatches "th" dict