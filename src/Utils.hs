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
  -- Equivalent of foldr for this AND
  (&&&!) :: (a -> b) -> [(a -> b)] -> (a -> b)
  (&&&!) ident fs = foldr (&&&) ident fs

-- | Used to composed two Reqs, takes the OR of them.  Hopefully can be
-- | extended to other types.
class Orable a b where
  (|||) :: (a -> b) -> (a -> b) -> (a -> b)
  -- Equivalent of foldr for this OR
  (|||!) :: (a -> b) -> [(a -> b)] -> (a -> b)
  (|||!) ident fs = foldr (|||) ident fs

-- | Ands two requirements together.
instance Andable Player Bool where
  r1 &&& r2 = (\x -> r1 x && r2 x)

-- | Ors two requirements together.
instance Orable Player Bool where
  r1 ||| r2 = (\x -> r1 x || r2 x)

-- | Applies two use effects, one after the other.
instance Andable Player GSTrans where
  u1 &&& u2 = (\x -> (u2 x) . (u1 x))

instance Andable GS GS where
  gt1 &&& gt2 = gt1 . gt2

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

getExitDirs :: Room -> [Dir]
getExitDirs (Room _ _ doors _) = map extractDir (Map.toList doors) where
  extractDir :: (Door, Room) -> Dir
  extractDir (Door _ dir _, _) = dir

-- | Standard set of responses for performing actions.  Must insert name at
-- | the front.
stdDisp :: DispResp
stdDisp (Go d)       = " left to the " ++ dirStr d ++ "."
stdDisp (Get o)      = " picked up a " ++ name o ++ "."
stdDisp (Drop o)     = " dropped a " ++ name o ++ "."
stdDisp (Use o)      = " used the " ++ name o ++ "."
stdDisp (Say s)      = " says: " ++ s
stdDisp (Yell y)     = " yells: " ++ fmap toUpper y
stdDisp Look         = " looks around for a bit."
stdDisp (LookAt n)   = " looks at " ++ show n ++ "."
stdDisp SeeAll       = " gets a birds-eye view of the entire map."
stdDisp (MkObj o)    = " made a new " ++ name o ++ "."
stdDisp (MkRm (Door dn dir _) r) = " made a " ++ dn ++ " " ++ 
                                   dirStr dir ++ " to the " ++
                                   name r ++ " room."
stdDisp (MkBag b)    = " made a new " ++ name b ++ "."
stdDisp (MkPlayer p) = " made a new player named " ++ name p ++ "... whoa!"
stdDisp (RmObj d)    = " removed a " ++ d ++ "."
stdDisp (RmDoor dir) = " remove a door to " ++ dirStr dir ++ "."
stdDisp (SetStat n k v) = " set " ++ n ++ "'s " ++ k ++ " to " ++ show v ++ "."
stdDisp (MustHaveStatLE n k v)  = " set the required " ++ k ++ " for " ++
                                 n ++ " to be at most " ++ show v ++ "."
stdDisp (MustHaveStatGE n k v)  = " set the required " ++ k ++ " for " ++
                                 n ++ " to be at least " ++ show v ++ "."
stdDisp (MustHaveNObjs n k v)   = " set the requirements for " ++ n ++
                                 " to at least " ++ show v ++ " " ++ k ++ "."
stdDisp (Teleports _ n)  = " added a teleport effect to " ++ n ++ "."
stdDisp (SetsStat n k v) = " added a stat effect to " ++ n ++ ", setting " ++
                           "the user's " ++ k ++ " to " ++ show v ++ "."
stdDisp Inv              = ""
stdDisp Stats            = ""
stdDisp Help             = ""
stdDisp Save             = " saved the world."
stdDisp Quit             = " lost the game."

-- | Displays the mappings between all rooms in the map, and the contents
-- | of each room.
mapStr :: GS -> String
mapStr (GS rooms) = intercalate "\n" $ fmap roomStr (Set.toList rooms)

-- | Displays the contents of a room and the doors leading out of it.
roomStr :: Room -> String
roomStr r@(Room n _ _ _) = n ++ ":\n" ++ exitStr r ++ "\n" ++ inventoryStr r

objStr :: (Objectable a) => a -> String
objStr o = name o ++ ": " ++ desc o ++ childStr where
  childStr = case contains o of 
    (_:_) -> " -- Contains (" ++ 
             intercalate ", " (fmap show (contains o)) ++ ")"
    []    -> ""

-- | ToString a container's inventory.
inventoryStr :: (Container c) => c -> String
inventoryStr c = intercalate "\n" strs where
  names = fmap show (contains c)
  strs  = fmap strItem (nub names) where
    strItem n = "X" ++ show (length $ filter (==n) names) ++ " " ++ n

-- | ToString the doors leading out of a room.
exitStr :: Room -> String
exitStr (Room _ _ doors _) = intercalate "\n" (toStr $ Map.toList doors) where
  toStr = fmap (\(d, _) -> doorStr d) where
    doorStr (Door n d _) = "  A " ++ n ++ " is to the " ++ dirStr d ++ "."

-- | Below are functions for modifying game state.

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

moveObjRms :: (Objectable a) => a -> Room -> Room -> GSTrans
moveObjRms o r1 r2 = (removeObj o r1) &&& (addObj o r2)

-- | Move an object from a room into a container.
getObj :: (Objectable a, Objectable b) => a -> Room -> b -> GSTrans
getObj o r b gs = (if hasObjChild b r && hasObjChild o r
                  then addRoom r'
                  else id) gs where
  r' = add (remove (remove r o) b) (add b o)

-- | Moves the object from the container to the room that container is in.
dropObj :: (Objectable a, Objectable b) => a -> b -> GSTrans
dropObj o b gs = (id &&&! map dropObjFn objRms) gs where
  objRms = findRmsWith b gs
  dropObjFn r = if hasObjChild o r then addRoom (modRoom r) else id
  modRoom r = add (add (remove r b) o) (add b o)

addObj :: (Objectable a) => a -> Room -> GSTrans
addObj o r = addObjs o [r]

addObjs :: (Objectable a) => a -> [Room] -> GSTrans
addObjs o rs (GS rooms) = GS (foldr addNewRm rooms rs) where
  addNewRm r = Set.insert (add r o)

removeObj :: (Objectable a) => a -> Room -> GSTrans
removeObj o r = removeObjs o [r]

removeObjs :: (Objectable a) => a -> [Room] -> GSTrans
removeObjs o rs (GS rooms) = GS (foldr addNewRm rooms rs) where
  addNewRm r = Set.insert (remove r o)

-- | Modifies all objects in the game world matching the first argument by
-- | the provided function.
modObjs :: (Objectable a, Objectable b) => a -> (a -> b) -> GSTrans
modObjs o f = replaceObjs o (f o)

modAdvObjs :: (Objectable a) => AdvObject -> (AdvObject -> a) -> GSTrans
modAdvObjs o f = replaceObjs o (f o)

addRoom :: Room -> GSTrans
addRoom r (GS rooms) = GS (Set.insert r rooms)

-- | Finds all occurrences of an object within a container.
findObj    :: (Objectable a, Container b) => a -> b -> [ThingBox]
findObj o b    = filter (==(TB o)) (contains b)

-- | Finds all rooms containing this object.
findRmsWith :: (Objectable a) => a -> GS -> [Room]
findRmsWith o (GS rooms) = foldr (\r -> if null $ findObj o r
                                       then id
                                       else (r:)) [] (Set.toList rooms)

findObjsByCond :: (Container a) => (ThingBox -> Bool) -> a -> [ThingBox]
findObjsByCond pred b = filter pred (contains b)

findGSObjsByCond :: (ThingBox -> Bool) -> GS -> [ThingBox]
findGSObjsByCond pred (GS rooms) = foldr (++) [] 
                                   (map onlySat $ Set.toList rooms) where
  onlySat :: Room -> [ThingBox]
  onlySat r = filter pred $ contains r

findAllObjNames :: GS -> [String]
findAllObjNames = map (\(TB o) -> name o) . findGSObjsByCond (const True)

-- | Returns all names in the same room.
findOtherNames :: Player -> GS -> [String]
findOtherNames p gs =  foldr (++) [] $ map allObjNames pRms where
  pRms = findRmsWith p gs
  allObjNames = map (\(TB o) -> name o) . findObjsByCond (const True)

findRms :: Room -> GS -> [Room]
findRms r (GS rooms) = foldr (\r' -> if r' == r
                                     then (r:)
                                     else id) [] (Set.toList rooms)

isSuper :: Player -> Bool
isSuper (Player _ _ _ _ _ sudo) = sudo

-- | 12/19/11 AB: TODO, must think of a better way to make Bag of class Usable.
-- | Problem is, it relies on functions in Utils, but importing Utils into
-- | Types would lead to mutual imports, which are not allowed.
instance Usable Bag where
  isUsable _ _ = True
  use b _ gs = foldr (\r (GS rooms') ->
                                (GS (Set.insert (dumpedRm r) rooms')))
                        gs bagRms where
    bagRms :: [Room]
    bagRms = findRmsWith b gs
    dumpedRm :: Room -> Room
    dumpedRm r = case b ==> r of
      (b', r') -> replace r' b b'
  getActionStr b = " dumped out the contents of " ++ name b ++ "."  
instance Objectable Bag
