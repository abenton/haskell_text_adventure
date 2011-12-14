module Types (
    -- list exported types here
) where 

-- | How to move between locations
data Exits = Exit {
    exitDir :: String,
    exitTo  :: Location
}

-- | The basic layout for a room
data Location = Loc {
    locName  :: String,
    locDesc  :: String,
    locExits :: [Exits]
}

-- | The whole world for the game
data AdvMap = AdvMap { 
    mapLocations :: [Location] 
}

-- | Things that can be moved and equiped
data Object = Obj {
    objName    :: String,
    objDesc    :: String,
    objPickup  :: String,
    objDiscard :: String
}

-- | The character that the end user is playing
data Player = Player {
    playerAt  :: Location,
    playerHas :: [Object] 
}

-- | All the non-player characters in the game
data Monster = Monster {
    monsterDesc :: String,
    monsterAt   :: Location,
    monsterHas  :: [Object]
}
