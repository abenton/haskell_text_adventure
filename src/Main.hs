module Main where

import Builder
import Char
import Data.Map as M
import Test.HUnit
import Types2

main :: IO ()
main = do 
       configPath <- getLine
       return ()

-- | Convert a command line input into a list of tokens, separated by spaces
tokenize :: String -> [String]
tokenize = words . Prelude.map toLower
       
tokenizeTest :: Test
tokenizeTest = "tokenizeTest" ~: TestList 
    [ tokenize ""                    ~?= [],
      tokenize "the QUICK Brown fOX" ~?= ["the","quick","brown","fox" ],
      tokenize "jumps over, huzzah!" ~?= ["jumps","over,","huzzah!"] ]

-- | Remove incidental words from token list
clean :: [String] -> [String]
clean = Prelude.filter (`notElem` ["a","the","to"])

cleanTest :: Test
cleanTest = "cleanTest" ~: TestList
    [ clean []                            ~?= [],
      clean ["the","quick","brown","fox"] ~?= ["quick","brown","fox"],
      clean ["a,","to","the","z"]         ~?= ["a,","z"] ]

-- Handle verbs that act on a Dir
-- TODO: remove code duplication
parseDir :: String -> [String] -> Maybe Action
parseDir verb [objects] = 
    case verb of 
        "go" -> case [objects] of
            ("north":_) -> Just (Go N)
            ("south":_) -> Just (Go S)
            ("east":_)  -> Just (Go E)
            ("west":_)  -> Just (Go W)
            ("up":_)    -> Just (Go U)
            ("down":_)  -> Just (Go D)
            _           -> Nothing
        "rmdoor" -> case [objects] of
            ("north":_) -> Just (RmDoor N)
            ("south":_) -> Just (RmDoor S)
            ("east":_)  -> Just (RmDoor E)
            ("west":_)  -> Just (RmDoor W)
            ("up":_)    -> Just (RmDoor U)
            ("down":_)  -> Just (RmDoor D)
            _           -> Nothing
parseDir _ _ = Nothing

-- | Handle verbs that act on an AdvObject
-- | TODO: check with game inventory to ensure object exists before acting on it
parseAdvObject :: String -> [String] -> Maybe Action
parseAdvObject verb (x:_) = 
    case verb of
        "get"   -> Just (Get o)
        "drop"  -> Just (Drop o)
        "use"   -> Just (Use o)
        "mkobj" -> Just (MkObj o)
        _       -> Nothing
    where o = mkObj x ""
parseAdvObject _ _ = Nothing

-- | Handle verbs that act on a Bag
parseBag :: String -> [String] -> Maybe Action
parseBag "mkbag" (x:_) = Just (MkBag (Bag x "" []))
parseBag _ _           = Nothing

-- | Handle verbs that act on a Player
parsePlayer :: String -> [String] -> Maybe Action
parsePlayer "mkplayer" (x:_) = 
    Just (MkPlayer (Player x "" M.empty [] True False))
parsePlayer _ _ = Nothing

-- | Handle verbs that act on a string
-- | TODO: check with game inventory to ensure object exists before acting on it
parseString :: String -> [String] -> Maybe Action
parseString verb (x:_) = 
    case verb of
        "rmobj" -> Just (RmObj x)
        _       -> Nothing
parseString _ _ = Nothing

-- | Handle verbs that act on a room and a string
parseRoomString :: String -> [String] -> Maybe Action
parseRoomString verb (x:xs) = 
    case verb of
        "teleports" -> Just (Teleports (Room x "" M.empty []) (unwords xs))
        _           -> Nothing
parseRoomString _ _ = Nothing

-- | Convert the user's text input into an action the game can execute
-- | TODO: create a map of allowable actions and their parsing functions to
-- |       make the code more generic
parse :: String -> Maybe Action
parse input = case verb of
    "go"        -> parseDir verb objects
    "get"       -> parseAdvObject verb objects
    "drop"      -> parseAdvObject verb objects
    "use"       -> parseAdvObject verb objects
    "mkobj"     -> parseAdvObject verb objects
    "mkrm"      -> error "undefined" -- TODO: complete this action
    "mkbag"     -> parseBag verb objects
    "mkplayer"  -> parsePlayer verb objects
    "rmobj"     -> parseString verb objects
    "rmdoor"    -> parseDir verb objects
    "setstat"   -> error "undefined" -- TODO: complete this action
    "teleports" -> error "undefined" -- TODO: complete this action
    "say"       -> Just (Say (unwords $ drop 1 $ words input))
    "yell"      -> Just (Yell (unwords $ drop 1 $ words input))
    "inv"       -> Just Inv
    "stats"     -> Just Stats
    "save"      -> Just Save
    "quit"      -> Just Quit
    _           -> Nothing
    where (verb:objects) = clean (tokenize input)

testHammer = mkObj "hammer" ""
testBag    = Bag "mybag" "" []

parseTest :: Test
parseTest = "parseTest" ~: TestList
    [ parse "Dude, where's my car?" ~?= Nothing,
      parse "Go West"               ~?= Just (Go W),
      parse "Go westerly"           ~?= Nothing,
      parse "Get hammer"            ~?= Just (Get testHammer),
      parse "Drop hammer"           ~?= Just (Drop testHammer),
      parse "Use hammer"            ~?= Just (Use testHammer),
      parse "MkObj hammer"          ~?= Just (MkObj testHammer),
      parse "MkBag myBag"           ~?= Just (MkBag testBag),
      parse "RmObj sticks & stones" ~?= Just (RmObj "sticks"),
      parse "RmDoor east"           ~?= Just (RmDoor E),
      parse "Say this is the time"  ~?= Just (Say "this is the time"),
      parse "Yell yodel-eh-he-hoo!" ~?= Just (Yell "yodel-eh-he-hoo!")
    ]    
    
allTests :: Test
allTests = TestList [ tokenizeTest, cleanTest, parseTest ]
