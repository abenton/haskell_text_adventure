module Utils where
import Types2
import Data.List (intercalate, nub)
import Data.Char (toUpper)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Contains helper methods for the rest of the engine.
getOppDir :: Dir -> Dir
getOppDir N = S
getOppDir S = N
getOppDir W = E
getOppDir E = W
getOppDir U = D
getOppDir D = U

dirStr :: Dir -> String
dirStr N = "north"
dirStr S = "south"
dirStr W = "west"
dirStr E = "east"
dirStr U = "up"
dirStr D = "down"

-- | Standard set of responses for performing actions.  Must insert name at
-- | the front.
stdDisp :: DispResp
stdDisp (Go d)       = " left to the " ++ dirStr d ++ "."
stdDisp (Get o)      = " picked up a " ++ name o ++ "."
stdDisp (Drop o)     = " dropped a " ++ name o ++ "."
stdDisp (Use o)      = " used the " ++ name o ++ "."
stdDisp (Say s)      = " says: " ++ s
stdDisp (Yell y)     = " yells: " ++ fmap toUpper y
stdDisp (MkObj o)    = " made a new " ++ name o ++ "."
stdDisp (MkRm (Door dn dir _) r) = " made a " ++ dn ++ " " ++ 
                                   dirStr dir ++ " to the " ++
                                   name r ++ " room."
stdDisp (MkBag b)    = " made a new " ++ name b ++ "."
stdDisp (MkPlayer p) = " made a new player named " ++ name p ++ "... whoa!"
stdDisp Quit         = " lost the game."

-- | ToString a container's inventory.
inventoryStr :: (Container c) => c -> String
inventoryStr c = intercalate "\n" strs where
  names = fmap show (contains c)
  strs  = fmap strItem (nub names) where
    strItem n = "X" ++ show (length $ filter (==n) names) ++ " " ++ n

-- | ToString the doors leading out of a room.
exitStr :: Room -> String
exitStr (Room _ _ doors _) = intercalate "\n" (toStr $ Map.toList doors) where
  toStr = fmap (\(d, r) -> doorStr d) where
    doorStr (Door n d _) = "A " ++ n ++ " is to the " ++ dirStr d ++ "."

-- | Finds all occurrences of an object within a container.
findObj    :: (Thing a, Container b) => a -> b -> [ThingBox]
findObj o b    = filter (==(TB o)) (contains b)

-- | Finds all occurrences of an object within the game.
findGSObj  :: (Thing a) => a -> GS -> [ThingBox]
findGSObj o (GS rooms) = foldr (\r -> ((findObj o r)++)) [] (Set.toList rooms)

-- | Finds all rooms containing this object.
findRmsWith :: (Thing a) => a -> GS -> [Room]
findRmsWith o (GS rooms) = foldr (\r -> if null $ findObj o r
                                       then id
                                       else (r:)) [] (Set.toList rooms)
