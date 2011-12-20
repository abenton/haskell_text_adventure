module Main where

import Char
import Types2
import Test.HUnit

main :: IO ()
main = do 
       configPath <- getLine
       return ()

-- | Convert a command line input into a list of tokens, separated by spaces
tokenize :: String -> [String]
tokenize = words . map toLower
       
tokenizeTest :: Test
tokenizeTest = "tokenizeTest" ~: TestList 
    [ tokenize ""                    ~?= [],
      tokenize "the QUICK Brown fOX" ~?= ["the","quick","brown","fox" ],
      tokenize "jumps over, huzzah!" ~?= ["jumps","over,","huzzah!"] ]

-- | Remove incidental words from token list
clean :: [String] -> [String]
clean = filter (`notElem` ["a","the","to"])

cleanTest :: Test
cleanTest = "cleanTest" ~: TestList
    [ clean []                            ~?= [],
      clean ["the","quick","brown","fox"] ~?= ["quick","brown","fox"],
      clean ["a,","to","the","z"]         ~?= ["a,","z"] ]

parseGo :: [String] -> Maybe Action
parseGo [objects] = case [objects] of
    ("north":_) -> Just (Go N)
    ("south":_) -> Just (Go S)
    ("east":_)  -> Just (Go E)
    ("west":_)  -> Just (Go W)
    ("up":_)    -> Just (Go U)
    ("down":_)  -> Just (Go D)
    _           -> Nothing

-- | Convert the user's text input into an action the game can execute
parse :: String -> Maybe Action
parse input = case verb of
    "go" -> parseGo objects
    _    -> Nothing
    where (verb:objects) = clean (tokenize input)
    
--parseTest :: Test
--parseTest = "parseTest" ~: TestList
--    [ parse "Go West" ~?= Just (Go W) ]    
    
allTests :: Test
allTests = TestList [ tokenizeTest, cleanTest ]
