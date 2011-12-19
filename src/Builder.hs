module Builder where

import Types2
import Data.Set as Set
import Data.Text (toUpper)

-- | Built-in types of requirements.
allowAll :: Req
allowAll _ = True

denyAll :: Req
denyAll _ = False

hasObj :: String -> Req
hasObj = hasNObj 1

hasNObj :: Int -> String -> Req
hasNObj n s p = length (filter ((==s) . name) (contains p)) >= n

isStrEq :: String -> String -> Req
isStrEq k v p = case getStrField p k of
  Just v' -> v == v'
  Nothing      == False

isStatLE :: Int -> String -> Req
isStatLE n s p = case getNumField p s of
  Just n' -> n' <= n
  Nothing -> True

isStatGE :: Int -> String -> Req
isStatGE n s p = case getNumField p s of
  Just n'          -> n' >= n
  Nothing | n <= 0 -> True
  Nothing          -> False

class Andable a b where
  (&&&) :: (a -> b) -> (a -> b) -> (a -> b)

class Orable a b where
  (|||) :: (a -> b) -> (a -> b) -> (a -> b)

-- | Ands two requirements together.
instance Andable Player Bool where
  r1 &&& r2 = (\x -> r1 x && r2 x)

-- | Ors two requirements together.
instance Orable Player Bool where
  r1 ||| r2 = (\x -> r1 x || r2 x)

-- | Applies two use effects, one after the other.
instance Andable Player (GS -> GS) where
  u1 &&& u2 = (\x -> (u2 x) . (u1 x))

-- | Built-in types of effects that items can do.
destroy :: 

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

mkRoom :: String -> String -> Room
mkRoom n d = Room n d Map.empty []

addOpenDoor :: Room -> Dir -> Room -> Room
addOpenDoor (Room n d m c) dir r2 = Room n d (Map.insert 
                                              (Door "door" dir allowAll) m) c

-- | The response that should be sent back to the client to be displayed.
type DispResp :: Action -> String

-- | Standard set of responses for performing actions.  Must insert name at
-- | the front.
stdDisp :: DispResp
stdDisp (Go d)       = " left to the " ++ dirStr d ++ "."
stdDisp (Get o)      = " picked up a " ++ name o ++ "."
stdDisp (Drop o)     = " dropped a " ++ name o ++ "."
stdDisp (Use o)      = " used the " ++ name o ++ "."
stdDisp (Say s)      = " says: " ++ s
stdDisp (Yell y)     = " yells: " ++ toUpper s
stdDisp (MkObj o)    = " made a new " ++ name o ++ "."
stdDisp (MkRm (Door dn dir _) r) = " made a " ++ dn ++ " " ++ 
                                                 dirStr dir ++ " to the " ++
                                                 name r ++ " room."
stdDisp (MkBag b)    = " made a new " ++ name b ++ "."
stdDisp (MkPlayer p) = " made a new player named " ++ name p "... whoa!"
stdDisp Quit         = " lost the game."

mkObj :: String -> String -> AdvObject
mkObj n d = AdvObject n d (" used a " ++ n ++ ".") allowAll id

addDoor :: Room -> Dir -> Req

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
