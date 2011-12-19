{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}

module Utils where
import Types2
import Data.List (intercalate, nub)
import Data.Char (toUpper)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Contains helper methods for the rest of the engine.

-- | Composes two arbitrary functions.  For Reqs, this takes the AND of
-- | their results.  For item uses, this transforms the game state with the
-- | first use, then transforms it again with the second use.
class Andable a b where
  (&&&) :: (a -> b) -> (a -> b) -> (a -> b)

-- | Used to composed two Reqs, takes the OR of them.  Hopefully can be
-- | extended to other types.
class Orable a b where
  (|||) :: (a -> b) -> (a -> b) -> (a -> b)

-- | Ands two requirements together.
instance Andable Player Bool where
  r1 &&& r2 = (\x -> r1 x && r2 x)

-- | Ors two requirements together.
instance Orable Player Bool where
  r1 ||| r2 = (\x -> r1 x || r2 x)

-- | Applies two use effects, one after the other.
instance Andable Player GSTrans where
  u1 &&& u2 = (\x -> (u2 x) . (u1 x))

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

-- | Returns the message to send to a player.  The first argument is the
-- | recipient, the doer of the action, the action done, and the mapping from
-- | actions to messages.
getDispStr :: Player -> Player -> Action -> DispResp -> String
getDispStr recipient doer action resp = (if recipient == doer
                                        then "You"
                                        else name doer) ++ resp action

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

hasObjChild :: (Objectable a, Container b) => a -> b -> Bool
hasObjChild t c = not $ null (findObj t c)

-- | Finds all occurrences of an object within the game.
findGSObj  :: (Objectable a) => a -> GS -> [ThingBox]
findGSObj o (GS rooms) = foldr (\r -> ((findObj o r)++)) [] (Set.toList rooms)

-- | Replaces one object with another in a room in the game state.  If either
-- | the original object or room is not in the game state, then just returns
-- | the game state.
replaceObj :: (Objectable a, Objectable b) => a -> b -> Room -> GSTrans
replaceObj o1 o2 r (GS rooms) = if (Set.member r rooms) && (hasObjChild o1 r)
                                then GS $ Set.insert (replace r o1 o2) rooms
                                else GS rooms

-- | Replaces all objects in the game world that match the first argument
-- | with the second object.
replaceObjs :: (Objectable a, Objectable b) => a -> b -> GSTrans
replaceObjs o1 o2 gs = foldr repRmTrans id objRms $ gs where
  repRm  = replaceObj o1 o2
  repRmTrans r gsTrans = (repRm r) . gsTrans
  objRms = findRmsWith o1 gs

-- | Modifies all objects in the game world matching the first argument by
-- | the provided function.
modObjs :: (Objectable a, Objectable b) => a -> (a -> b) -> GSTrans
modObjs o f = replaceObjs o (f o)

-- | Finds all occurrences of an object within a container.
findObj    :: (Objectable a, Container b) => a -> b -> [ThingBox]
findObj o b    = filter (==(TB o)) (contains b)

-- | Finds all rooms containing this object.
findRmsWith :: (Objectable a) => a -> GS -> [Room]
findRmsWith o (GS rooms) = foldr (\r -> if null $ findObj o r
                                       then id
                                       else (r:)) [] (Set.toList rooms)

-- | 12/19/11 AB: TODO, must think of a better way to make Bag of class Usable.
-- | Problem is, it relies on functions in Utils, but importing Utils into
-- | Types would lead to mutual imports, which are not allowed.
-- | Using a bag should dump out all its contents.
instance Usable Bag where
  isUsable _ _ = True
  use b p gs@(GS rooms) = foldr (\r (GS rooms') ->
                                (GS (Set.insert (dumpedRm r) rooms')))
                        gs bagRms where
    bagRms :: [Room]
    bagRms = findRmsWith b gs
    dumpedRm :: Room -> Room
    dumpedRm r = case b ==> r of
      (b', r') -> replace r' b b'
  getActionStr b = " dumped out the contents of " ++ name b ++ "."  
instance Objectable Bag
