module Main where

import Char
import Test.HUnit
import Types2

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

-- | Convert the user-supplied direction into the corresponding Dir type
parseDir :: [String] -> Maybe Action
parseDir [objects] = case [objects] of
    ("north":_) -> Just (Go N)
    ("south":_) -> Just (Go S)
    ("east":_)  -> Just (Go E)
    ("west":_)  -> Just (Go W)
    ("up":_)    -> Just (Go U)
    ("down":_)  -> Just (Go D)
    _           -> Nothing

parseAdvObject :: [String] -> Maybe Action
parseAdvObject [objects] = error "parseAdvObject is not yet defined"

-- | Convert the user's text input into an action the game can execute
parse :: String -> Maybe Action
parse input = case verb of
    "go"   -> parseDir objects
    "get"  -> parseAdvObject objects
    "drop" -> parseAdvObject objects
    "use"  -> parseAdvObject objects
    "say"  -> Just (Say (unwords $ drop 1 $ words input))
    "yell" -> Just (Yell (unwords $ drop 1 $ words input))
    _    -> Nothing
    where (verb:objects) = clean (tokenize input)
    
parseTest :: Test
parseTest = "parseTest" ~: TestList
    [ parse "Go West" ~?= Just (Go W),
      parse "Say this is the time" ~?= Just (Say "this is the time"),
      parse "Yell yodel-eh-he-hoo!" ~?= Just (Yell "yodel-eh-he-hoo!")
    ]    
    
allTests :: Test
allTests = TestList [ tokenizeTest, cleanTest, parseTest ]
