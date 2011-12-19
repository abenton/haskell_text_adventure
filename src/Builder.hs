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

-- | Built-in types of effects for items.  All return a Player -> GSTrans.
--setStat :: String -> Int -> Player -> GSTrans
--setStat k v p = error "undefined"

mkRoom :: String -> String -> Room
mkRoom n d = Room n d Map.empty []

addOpenDoor :: Room -> Dir -> Room -> Room
addOpenDoor r1 dir r2 = addExit r1 "door" dir allowAll r2

mkObj :: String -> String -> AdvObject
mkObj n d = AdvObject n d (" used a " ++ n ++ ".") allowAll (\_ -> id)

addExit :: Room -> String -> Dir -> Req -> Room -> Room
addExit (Room n d m c) dn dir req r2 = Room n d (Map.insert
                                                 (Door dn dir req) r2 m) c

emptyGS :: GS
emptyGS = GS Set.empty

addObj :: AdvObject -> Room -> Room
addObj o (Room n d m c) = Room n d m ((TB o):c)

biconnect :: GS -> Room -> Dir -> Room -> GS
biconnect = error "undefined" where
  biconnectRooms :: Room -> Dir -> Room -> (Room, Room)
  biconnectRooms = error "undefined"

--biconnect gs@(GS rs clients) r1 d r2 = if (Set.member r1 rs) && 
--                                          (Set.member r2 rs)
--                                       then GS (Set.insert (Set.insert rs (addOpenDoor r1 d r2) (addOpenDoor r2 (getOppDir d) r1))) r1 d r2
--                                       else gs

-- | Sample game state.
room1 :: Room
room1 = mkRoom "start" "You are at the start of the demo map"

room2 :: Room
room2 = mkRoom "end" "You are at the end of the demo map"

room3 :: Room
room3 = mkRoom "other" "You are in another room in the map"
