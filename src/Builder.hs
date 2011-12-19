module Builder where

import Types2
import Data.Map as Map
import Data.Set as Set

            "exits": 
            {
            },
            "objects": ["item1", "item2"]
        },
        {
            "exits": 
            {
            },
            "objects": []
        },
        {
            "exits": 
            {
            },
            "objects": []
        }
    ],


allowAll :: Req
allowAll _ = True

denyAll :: Req
denyAll _ = True

getOppDir :: Dir -> Dir
getOppDir N = S
getOppDir S = N
getOppDir W = E
getOppDir E = W
getOppDir U = D
getOppDir D = U

mkRoom :: String -> String -> Room
mkRoom n d = Room n d Map.empty []

addOpenDoor :: Room -> Dir -> Room -> Room
addOpenDoor (Room n d m c) dir r2 = Room n d (Map.insert 
                                              (Door "door" dir allowAll) m) c

emptyGS :: GS
emptyGS = GS Set.empty []

addObj :: AdvObject -> Room -> Room
addObj o (Room n d m c) = Room n d m (o:c)

biconnect :: GS -> Room -> Dir -> Room -> GS
biconnect gs@(GS rs clients) r1 d r2 = if (member r1 rs) && (member r2 rs)
                                       then GS (Set.insert (Set.insert rs (addOpenDoor r1 d r2) (addOpenDoor r2 (getOppDir d) r1))) r1 d r2
                                       else gs



-- | Sample game state.
room1 :: Room
room1 = biconnect (mkRoom "start" "You are at the start of the demo map"
                "north": "end"
                "south": "other"



room2 :: Room
room2 = mkRoom "end" "You are at the end of the demo map"

room3 :: Room
room3 = mkRoom "other" "You are in another room in the map"
                "north": "start"
