{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Builder where

import Utils
import Types2
import Data.Map as Map
import Data.Set as Set
import Data.Text (toUpper)

-- | Built-in types of requirements.
allowAll :: Req
allowAll _ = True

denyAll :: Req
denyAll _ = False

hasObj :: String -> Req
hasObj = hasNObj 1

-- | Need n of a certain object to use object or go through exit. 
hasNObj :: Int -> String -> Req
hasNObj n s p = length (Prelude.filter filterFn (contains p)) >= n where
  filterFn (TB b) = name b == s

isStrEq :: String -> String -> Req
isStrEq k v p = case getStrField p k of
  Just v' -> v == v'
  Nothing -> False

-- | Is a stat less than or equal to a threshold.
isStatLE :: Int -> String -> Req
isStatLE n s p = case getNumField p s of
  Just n' -> n' <= n
  Nothing -> True

isStatGE :: Int -> String -> Req
isStatGE n s p = case getNumField p s of
  Just n'          -> n' >= n
  Nothing | n <= 0 -> True
  Nothing          -> False

-- | Built-in types of effects for items.  All return a Player -> GSTrans.
removeEffects :: (Objectable a) => a -> a
removeEffects o = setEffect o noEffect

addEffect :: (Objectable a) => (Player -> GSTrans) -> a -> a
addEffect effect o = setEffect o (effect &&& (use o)) where

noEffect :: (Objectable a) => a -> GSTrans
noEffect _ = id

setStat :: (Objectable a) => String -> Int -> a -> GSTrans
setStat k v o = replaceObjs (setNumField k v o) o

-- | Moves all occurrences of the object to another room.
teleports :: (Objectable a) => Room -> a -> GSTrans
teleports r o gs = (addTrans &&& remTrans) gs where
  srcRms = findRmsWith o gs
  remTrans = removeObjs o srcRms
  addTrans = addObj o r

-- | Object will destroy itself on use.
consumable :: (Objectable a) => a -> Player -> GSTrans
consumable o p gs = replaceObjs p (remove o p) $ gs

mkConsumable :: (Objectable a) => a -> a
mkConsumable o = addEffect (consumable o) o

-- | Removes all occurrences of this object.
destroy :: (Objectable a) => a -> GSTrans
destroy o gs = removeObjs o (findRmsWith o gs) $ gs

-- | Makes N copies of the same object, each with a unique number appended
-- | to the end of their names.
makeN :: (Objectable a) => a -> Int -> GSTrans
makeN o n gs | n > 1 = (id &&&! [addObjRms i | i <- [1..(n-1)]]) gs where
  objRms = findRmsWith o gs
  addObjRms :: Int -> GSTrans
  addObjRms i = addObjs (setName (name o ++ show i) o) objRms
makeN _ _ gs = gs

-- | Functions for creating rooms/doors/objects.
mkRoom :: String -> String -> Room
mkRoom n d = Room n d Map.empty []

addOpenDoor :: Room -> Dir -> Room -> Room
addOpenDoor r1 dir r2 = addExit r1 "door" dir allowAll r2

mkObj :: String -> String -> AdvObject
mkObj n d = AdvObject n d (" used a " ++ n ++ ".") allowAll noEffect

mkPlayer :: String -> String -> Sudo -> Player
mkPlayer name desc = Player name desc Map.empty [] True

addExit :: Room -> String -> Dir -> Req -> Room -> Room
addExit (Room n d m c) dn dir req r2 = Room n d (Map.insert
                                                 (Door dn dir req) r2 m) c

addExit' :: Room -> Door -> Room -> Room
addExit' r1 d@(Door n dir req) r2 = addExit r1 n dir req r2

getOppRoom :: Room -> Door -> Maybe Room
getOppRoom r@(Room _ _ m _) d = Map.lookup d m

setDoorReq :: Req -> Door -> Door
setDoorReq req (Door n dir _) = Door n dir req

setDoorDir :: Dir -> Door -> Door
setDoorDir dir (Door n _ req) = Door n dir req

setDoorName :: String -> Door -> Door
setDoorName n d@(Door _ dir req) = Door n dir req

-- | If there is no corresponding room opposite this one, then game state is
-- | unchanged.  If there is no corresponding door in the opposite room, then 
-- | a copy of this door is placed in the opposite room.
modBiDoor :: (Door -> Door) -> Room -> Door -> GSTrans
modBiDoor f r1 d@(Door _ dir _) gs = (case (r1', r2') of
                  (Just x, Just y) -> ((addRoom x) &&& (addRoom y)) gs
                  (_, _) -> gs) where
  d1' = f d
  d2' = setDoorDir (getOppDir dir) d1'
  r1' = fmap (addExit' r1 d1') oppRm
  r2' = fmap (\r2 -> addExit' r2 d2' r1) oppRm
  oppRm = getOppRoom r1 d

setBiDoorName :: Room -> Door -> String -> GSTrans
setBiDoorName r1 d n' = modBiDoor (setDoorName n') r1 d

-- | Sets the requirement of the door on both sides.  If there is no
-- | corresponding door in the opposite room, then that room is unchanged.
setBiDoorReq :: Room -> Door -> Req -> GSTrans
setBiDoorReq r1 d req' = modBiDoor (setDoorReq req') r1 d

gsHasRoom :: Room -> GS -> Bool
gsHasRoom r (GS rooms) = Set.member r rooms

addRoom :: Room -> GSTrans
addRoom r gs@(GS rooms) = GS (Set.insert r rooms)

-- | Connect two rooms with an open door.  If either room is not in the
-- | game state, then it will be added to it.
biconnect :: Room -> Dir -> Room -> GSTrans
biconnect r1 dir r2 gs = (addRoom r1') &&& (addRoom r2') $ gs where
  rmsExist = (gsHasRoom r1 gs) && (gsHasRoom r2 gs)
  r2Exists = gsHasRoom
  r1' = addOpenDoor r1 dir r2
  r2' = addOpenDoor r2 (getOppDir dir) r1

-- | Sample game state.
emptyGS :: GS
emptyGS = GS Set.empty

-- | Teleporter to room2.
teleporter :: AdvObject
teleporter = addEffect (teleports room2) o where
  o = setUsable (mkObj itemName itemDesc) (isStatGE 40 "haskell_proficiency")
  itemName = "simple teleporter"
  itemDesc = "Nothing but a simple teleporter"

-- | Power bar, brings great power to the eater.
powerBar :: AdvObject
powerBar = addEffect upStrength (addEffect upHaskell o) where
  upStrength = setStat "strength" 42
  upHaskell  = setStat "haskell_proficiency" 42
  o = mkObj itemName itemDesc
  itemName = "power bar"
  itemDesc = "With great power comes great responsibility"

-- | Pointy stick, try not to hurt yourself
pointyStick :: AdvObject
pointyStick = addEffect ouch o where
  ouch = destroy
  o = setUsable (mkObj itemName itemDesc) (isStatGE 40 "strength")
  itemName = "extremely pointy stick"
  itemDesc = "Try not to hurt yourself with this.  Good thing it's child-proof"

-- | Obnoxious NPC.  Doesn't get really annoying until his AI is attached.
orneryNPC :: Player
orneryNPC = mkPlayer npcName npcDesc False where
  npcName = "grumpy gnome"
  npcDesc = "This gnome comes with a permanent frown." ++
            "Good thing he's not a super-user"

room1 :: Room
room1 =  add baseRm teleporter where
  baseRm = mkRoom "start" "you are at the start of the demo map"

room2 :: Room
room2 = add (add baseRm powerBar) orneryNPC where
  baseRm = mkRoom "end" "You are at the end of the demo map"

room3 :: Room
room3 = add baseRm pointyStick where
  baseRm = mkRoom "other" "You are in another room in the map"

sampleGS :: GS
sampleGS = ((biconnect room1 E room3) &&& (biconnect room1 N room2)) emptyGS
