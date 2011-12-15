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
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Attoparsec
import qualified Data.ByteString as B

data AdvRoom = AdvRoom {
    roomName  :: String,
    roomDesc  :: String
} deriving (Show)

deriveJSON id ''AdvRoom

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

-- | The config file used to load and save the game state
data AdvConfig = AdvConfig {
    locations :: [AdvLocation],
    objects   :: [AdvObject],
    players   :: [AdvPlayer],
    monsters  :: [AdvMonster]    
} deriving (Show)

deriveJSON id ''AdvConfig

parseAdvConfigFile = fmap parseAdvConfigData . B.readFile

parseAdvConfigData :: B.ByteString -> Data.Attoparsec.Result (Data.Aeson.Result [AdvConfig])
parseAdvConfigData content = parse (fmap fromJSON json) content
