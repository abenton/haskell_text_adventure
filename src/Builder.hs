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
noEffect :: (Objectable a) => a -> GSTrans
noEffect _ = id

setStat :: (Objectable a) => String -> Int -> a -> GSTrans
setStat k v o = replaceObjs (setNumField k v o) o

-- | Move all occurrences of the object to another room.
teleport :: (Objectable a) => Room -> a -> GSTrans
teleport r o gs = (addTrans &&& remTrans) gs where
  srcRms = findRmsWith o gs
  remTrans = removeObjs o srcRms
  addTrans = addObj o r

-- | Removes all occurrences of this object.
destroy :: (Objectable a) => a -> GSTrans
destroy o gs = removeObjs o (findRmsWith o gs) $ gs

-- | Makes N copies of the same object.  They are considered the same
-- | object, so any commands directed towards them will apply to all.
makeN :: (Objectable a) => a -> Int -> GSTrans
makeN o n gs | n > 1 = (id &&&! [addObjRms | i <- [1..(n-1)]]) gs where
  objRms = findRmsWith o gs
  addObjRms :: GSTrans
  addObjRms = addObjs o objRms
makeN _ _ gs = gs

-- | Functions for creating rooms/doors/objects.
mkRoom :: String -> String -> Room
mkRoom n d = Room n d Map.empty []

addOpenDoor :: Room -> Dir -> Room -> Room
addOpenDoor r1 dir r2 = addExit r1 "door" dir allowAll r2

mkObj :: String -> String -> AdvObject
mkObj n d = AdvObject n d (" used a " ++ n ++ ".") allowAll noEffect

addExit :: Room -> String -> Dir -> Req -> Room -> Room
addExit (Room n d m c) dn dir req r2 = Room n d (Map.insert
                                                 (Door dn dir req) r2 m) c

emptyGS :: GS
emptyGS = GS Set.empty

setDoorReq :: Door -> Req -> Door
setDoorReq (Door n dir _) req = Door n dir req

-- | Sets the requirement of the door on both sides.  If there is no
-- | corresponding door in the opposite room, then that room is unchanged.
setBiDoorReq :: Room -> Door -> Req -> GSTrans
setBiDoorReq = error "undefined"

setDoorName :: Door -> String -> Door
setDoorName (Door _ dir req) n = Door n dir req

setBiDoorName :: Room -> Door -> String -> GSTrans
setBiDoorName = error "undefined"

getGSRooms :: GS -> Set Room
getGSRooms (GS rooms) = rooms

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
room1 :: Room
room1 = mkRoom "start" "You are at the start of the demo map"

room2 :: Room
room2 = mkRoom "end" "You are at the end of the demo map"

room3 :: Room
room3 = mkRoom "other" "You are in another room in the map"
