module Main where

import Builder
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

parseDir :: String -> [String] -> Maybe Action
parseDir "go" [objects] = 
    case [objects] of
        ("north":_) -> Just (Go N)
        ("south":_) -> Just (Go S)
        ("east":_)  -> Just (Go E)
        ("west":_)  -> Just (Go W)
        ("up":_)    -> Just (Go U)
        ("down":_)  -> Just (Go D)
        _           -> Nothing
parseDir _ _ = Nothing

parseAdvObject :: String -> [String] -> Maybe Action
parseAdvObject verb (object:_) = 
    case verb of
        "get"   -> Just (Get o)
        "drop"  -> Just (Drop o)
        "use"   -> Just (Use o)
        "mkobj" -> Just (MkObj o)
        _       -> Nothing
    where o = mkObj object ""
parseAdvObject _ _ = Nothing

-- | Convert the user's text input into an action the game can execute
-- | TODO: create a map of allowable actions and their parsing functions to
-- |       make the code more generic
parse :: String -> Maybe Action
parse input = case verb of
    "go"    -> parseDir verb objects
    "get"   -> parseAdvObject verb objects
    "drop"  -> parseAdvObject verb objects
    "use"   -> parseAdvObject verb objects
    "mkobj" -> parseAdvObject verb objects
    "say"   -> Just (Say (unwords $ drop 1 $ words input))
    "yell"  -> Just (Yell (unwords $ drop 1 $ words input))
    "inv"   -> Just Inv
    "stats" -> Just Stats
    "save"  -> Just Save
    "quit"  -> Just Quit
    _       -> Nothing
    where (verb:objects) = clean (tokenize input)

testHammer = mkObj "hammer" ""

parseTest :: Test
parseTest = "parseTest" ~: TestList
    [ parse "Dude, where's my car?" ~?= Nothing,
      parse "Go West"               ~?= Just (Go W),
      parse "Go westerly"           ~?= Nothing,
      parse "Get hammer"            ~?= Just (Get testHammer ),
      parse "Drop hammer"           ~?= Just (Drop testHammer ),
      parse "Use hammer"            ~?= Just (Use testHammer ),
      parse "MkObj hammer"          ~?= Just (MkObj testHammer ),
      parse "Say this is the time"  ~?= Just (Say "this is the time"),
      parse "Yell yodel-eh-he-hoo!" ~?= Just (Yell "yodel-eh-he-hoo!")
    ]    
    
allTests :: Test
allTests = TestList [ tokenizeTest, cleanTest, parseTest ]
