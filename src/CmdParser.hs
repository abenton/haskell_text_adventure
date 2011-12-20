{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module CmdParser (
  helpStr,
  allParserTests,
  parseErrMsg,
  parse
) where

import Types2
import Builder
import Data.List (intercalate)
import Char
import Data.Map as M
import Test.HUnit

-- | The help message that pops up when asking for help.
helpStr :: String
helpStr = "Moving: go (north|south|east|west|up|down)\n" ++
          "Picking up: get \"obj_name\"\n" ++
          "Dropping: drop \"obj_name\"\n" ++
          "Using: use \"obj_name\"" ++
          "Say: say \"blah\"\n" ++
          "Yell: yell \"BLAH\"\n" ++
          "Check inventory: inv\n" ++
          "Check stats: stats\n" ++
          "Join the game: addme \"player_name\"\n" ++
          "Print command list: help\n\n" ++
          "----For super-users only----\n" ++
          "Make an object: mkobj \"obj_name\" \"desc\"\n" ++
          "Make a room: mkrm \"direction\" \"room_name\" \"desc\"\n" ++
          "Make a bag: mkbag \"bag_name\" \"desc\"\n" ++
          "Make a player: mkplayer \"player_name\" \"desc\"\n" ++
          "Remove object: rmobj \"obj_name\"\n" ++
          "Remove door: rmdoor \"dir\"\n" ++
          "Add teleport effect: teleports \"room_name\" \"obj_name\"\n"

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

dirMapping :: String -> Maybe Dir
dirMapping "north" = Just N
dirMapping "south" = Just S
dirMapping "east"  = Just E
dirMapping "west"  = Just W
dirMapping "up"    = Just U
dirMapping "down"  = Just D
dirMapping _       = Nothing

-- Handle verbs that act on a Dir
parseDir :: String -> [String] -> Maybe Action
parseDir verb [objects] = 
  case [objects] of
    (dirStr:rest) -> case verb of
      "go"     -> fmap Go (dirMapping dirStr)
      "rmdoor" -> fmap RmDoor (dirMapping dirStr)
      "mkroom" -> case rest of
        (n:d:_) -> fmap mkRmFn (dirMapping dirStr) where
          mkRmFn dir = MkRm (mkDoor dir) $ mkRoom n d
        _       -> Nothing
      _        -> Nothing
    _          -> Nothing
parseDir _ _ = Nothing

-- | Handle verbs that act on an AdvObject
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
parseBag "mkbag" (n:rest) = Just (MkBag (Bag n (intercalate " " rest) []))
parseBag _ _           = Nothing

-- | Handle verbs that act on a Player
parsePlayer :: String -> [String] -> Maybe Action
parsePlayer "mkplayer" (n:rest) = 
    Just (MkPlayer (Player n (intercalate " " rest) M.empty [] True False))
parsePlayer _ _ = Nothing

-- | Handle verbs that act on a string
parseString :: String -> [String] -> Maybe Action
parseString verb (x:_) = 
    case verb of
        "rmobj"   -> Just (RmObj x)
        "addme"   -> Just (AddMe x)
        _         -> Nothing
parseString _ _ = Nothing

-- | Handle verbs that act on a room and a string
parseRoomString :: String -> [String] -> Maybe Action
parseRoomString verb (x:xs) = 
    case verb of
        "teleports" -> Just (Teleports (Room x "" M.empty []) $ unwords xs)
        _           -> Nothing
parseRoomString _ _ = Nothing

parseErrMsg :: String
parseErrMsg = "Could not parse this command.  Type \"help\" to get" ++  
              "a list of possible commands."

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
    "mkrm"      -> parseString verb objects
    "mkbag"     -> parseBag verb objects
    "mkplayer"  -> parsePlayer verb objects
    "rmobj"     -> parseString verb objects
    "rmdoor"    -> parseDir verb objects
    "setstat"   -> Nothing -- TODO: Define these
    "setsstat"  -> Nothing -- TODO: Define these
    "teleports" -> parseRoomString verb objects
    "say"       -> Just (Say (unwords $ drop 1 $ words input))
    "yell"      -> Just (Yell (unwords $ drop 1 $ words input))
    "inv"       -> Just Inv
    "stats"     -> Just Stats
    "addme"     -> parseString verb objects
    "help"      -> Just Help
    "save"      -> Just Save
    "quit"      -> Just Quit
    _           -> Nothing
    where 
      verb = case clean (tokenize input) of
        (v:_)  -> v
        _      -> ""
      objects = case clean (tokenize input) of
        (_:os) -> os
        _      -> []

testHammer :: AdvObject
testHammer = mkObj "hammer" ""

testBag :: Bag
testBag = Bag "mybag" "" []

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

allParserTests :: Test
allParserTests = TestList [ tokenizeTest, cleanTest, parseTest ]
