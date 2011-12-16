{-# LANGUAGE TemplateHaskell #-}

-- Note: the JSON parsing code is based on code found at 
-- http://stackoverflow.com/questions/6930944/haskell-aeson-json-parsing-into-custom-type

module Types (
    -- Exported data types
    AdvRoom,
    AdvExit,
    AdvLocation,
    AdvObject,
    AdvPlayer,
    AdvMonster,
    AdvConfig
) where 

import Control.Monad
import Data.Aeson as Aes
import Data.Aeson.TH (deriveJSON)
import Data.Attoparsec as Ap
import qualified Data.ByteString as B

data AdvRoom = AdvRoom {
    roomName  :: String,
    roomDesc  :: String
} deriving (Show)

deriveJSON id ''AdvRoom

-- | Currently unused.  Not sure if good or bad idea.
data Dir = N | S | E | W | U | D deriving (Show)

deriveJSON id ''Dir

-- | How to move between locations in a game
data AdvExit = AdvExit {
    exitDir :: String,
    exitTo  :: AdvRoom
} deriving (Show)

deriveJSON id ''AdvExit

-- | The basic layout for a room
data AdvLocation = AdvLocation {
    locRoom  :: AdvRoom,
    locExits :: [AdvExit]
} deriving (Show)

deriveJSON id ''AdvLocation

-- | Things that can be moved and equiped
data AdvObject = AdvObject {
    objName    :: String,
    objDesc    :: String,
    objPickup  :: String,
    objDiscard :: String
} deriving (Show)

deriveJSON id ''AdvObject

-- | The character that the end user is playing
data AdvPlayer = AdvPlayer {
    playerAt  :: AdvLocation,
    playerHas :: [AdvObject] 
} deriving (Show)

deriveJSON id ''AdvPlayer

-- | All the non-player characters in the game
data AdvMonster = AdvMonster {
    monsterDesc :: String,
    monsterAt   :: AdvLocation,
    monsterHas  :: [AdvObject]
} deriving (Show)

deriveJSON id ''AdvMonster

-- | 12/16/11 AB: May be used later.
data AdvMap = AdvMap {
  locations :: [AdvLocation],
  objects   :: [AdvObject]
} deriving (Show)

-- | The config file used to load and save the game state
data AdvConfig = AdvConfig {
    locations :: [AdvLocation],
    objects   :: [AdvObject],
    players   :: [AdvPlayer],
    monsters  :: [AdvMonster]
} deriving (Show)

deriveJSON id ''AdvConfig

parseAdvConfigFile :: FilePath -> IO (Either String AdvConfig)
parseAdvConfigFile = fmap parseAdvConfigData . B.readFile

--parseAdvConfigData :: B.ByteString -> Ap.Result (Aes.Result AdvConfig)
--parseAdvConfigData content = parse (fmap fromJSON json) content

-- | 12/16/11 AB: Changed result type to a single configuration, not list of.
-- | Now returns an Either instead of Result within Result, easier to work with.
parseAdvConfigData :: B.ByteString -> Either String AdvConfig
parseAdvConfigData content = (case parseRes content of
                            Just (Success c) -> Right c
                            Just (Error   s) -> Left  s
                            Nothing          -> Left "Parse troubles...") where
  parseRes :: B.ByteString -> Maybe (Aes.Result AdvConfig)
  parseRes = maybeResult . parse (fmap fromJSON json)
