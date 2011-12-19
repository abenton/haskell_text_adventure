{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell, ExistentialQuantification #-}

-- Note: the JSON parsing code is based on code found at 
-- http://stackoverflow.com/questions/6930944/haskell-aeson-json-parsing-into-custom-type

module Types2 (
    -- Exported data types
    Dir(..),
    Action(..),
    Door(..),
    GS(..),
    Room,
    AdvObject,
    Bag,
    Player,
    Req
) where 

import Data.List as L
import Data.Set as Set
import Data.Map as Map
import Control.Concurrent.MVar

-- | Possible actions that a player can issue.
data Action = Go Dir
              | Get   AdvObject
              | Drop  AdvObject
              | Use   AdvObject
              | Say   String
              | Yell  String
              | MkObj AdvObject
              | MkRm  Door Room
              | MkBag Bag
              | MkPlayer Player
              | Quit

-- | 12/17/11 AB: May expand this later.  Thought it good to keep a finite set
-- | of directions to start with.
data Dir = N | S | E | W | U | D deriving (Show, Eq)

-- | 12/17/11 AB: A door is a generic term for an exit.  Takes a short 
-- | description/name, direction, and a precondition for opening as arguments.
data Door = Door String Dir Req

-- | 12/17/11 AB: Represents a precondition for performing an action.
-- | Dependent solely on the state of the Player.
type Req = Player -> Bool

-- | 12/17/11 AB: The current game state.  A map is just a set of rooms.  Also
-- | includes a mapping from Clients to Things in the game world.  These
-- | Clients can be PCs controlled by a person, or NPCs controlled by an AI.
data GS = GS (Set Room) [(MVar Action, Player)]

-- | Game state transformer
type GSTrans = GS -> GS

class (Thing a) => Container a where
  contains :: a -> [ThingBox]
  add      :: (Thing b) => a -> b -> a
  remove   :: (Thing b) => a -> b -> a
  move     :: (Container b, Thing c) => a -> c -> b -> (a, b)
  move src t dst = (remove src t, add dst t)
  moveAll  :: (Container b) => a -> b -> (a, b)
  moveAll src dst = foldr moveFn (src,dst) (contains src) where
    moveFn (TB a) (s', d') = move s' a d'
  (==>)    :: (Container b) => a -> b -> (a, b)
  src ==> dst = moveAll src dst

-- | 12/17/11 AB: Things encompass just about everything in the game world.
-- | Rooms, objects, PCs, NPCs.  The flexibility that Thing provides allows
-- | interesting game play, like picking up a gnome NPC and dropping it in
-- | your bag.  Also allows any object in the game to have a client attached
-- | to it, that can issue actions to the game engine.
class (Eq a, Show a) => Thing a where
  -- | All objects must have a name.  This name is used to test for equality.
  name :: a -> String
  -- | Objects may/may not have a description, which is what will be displayed
  -- | to the user.
  desc :: a -> String
  desc _ = ""
  -- | When Things are inactive, then they are unable to interact with the
  -- | game world.
  inactive :: a -> Bool
  inactive = not . active
  active   :: a -> Bool
  active   = not . inactive
  
  getNumField :: a -> String -> Maybe Int
  getNumField _ _ = Nothing
  getStrField :: a -> String -> Maybe String
  getStrField t "name" = Just $ name t
  getStrField t "desc" = Just $ desc t
  getStrField _ _ = Nothing

-- | Wrapper around thing types.  This is to allow rooms to contain
-- | bags, players, and objects.  Same goes for other containers.
data ThingBox = forall t. Thing t => TB t
instance Eq ThingBox where
  (TB a) == (TB b) = name a == name b

-- | Implemented by objects that can be put into inventory.
class (Thing a) => Takeable a where
  canTake :: a -> Req
  canTake _ _ = True

-- | String response to performing an action.
-- type DispResp = Action -> String

class (Thing a) => Usable a where
  isUsable     :: a -> Req
  isUsable   _ _ = True
  use          :: a -> Player -> GSTrans
  use        _ _ = id
  getActionStr :: a -> String
  getActionStr a = "Something happened because of " ++ (name a) ++ "."

-- | 12/17/11 AB: A room.  Pass it a unique name, description, mapping from
-- | doors to other rooms, and a list of children.
data Room = Room String String [(Door, Room)] [ThingBox]
instance Thing Room where
  name (Room n _ _ _)     = n
  desc (Room _ d _ _)     = d
  active _                = True
instance Container Room where
  contains (Room _ _ _ c) = c
  add (Room n d e c) t    = Room n d e ((TB t):c)
  remove (Room n d e c) t = Room n d e (L.delete (TB t) c)
instance Eq Room where
  r1 == r2 = name r1 == name r2
instance Ord Room where
  r1 <= r2 = name r1 <= name r2
instance Show Room where
  show r = name r ++ ": " ++ desc r

-- | Statistics for a character, can be set arbitrarily.
type Stats = Map String Int

-- | Flags this player as a super-user.  Has access to additional commands.
type Sudo = Bool

-- | Player: has a name, description, stats, inventory, and whether they are
-- | active.  PC indistinguishable from an NPC.  Only difference is the Client
-- | controlling it.
data Player = Player String String Stats [ThingBox] Bool Sudo
instance Thing Player where
  name (Player n _ _ _ _ _) = n
  desc (Player _ d _ _ _ _) = d
  active (Player _ _ _ _ a _) = a
  getNumField (Player _ _ s _ _ _) k = Map.lookup k s
instance Container Player where
  contains (Player _ _ _ c _ _) = c
  add (Player n d s c a su) o = Player n d s ((TB o):c) a su
  remove (Player n d s c a su) o = Player n d s (L.delete (TB o) c) a su
instance Takeable Player where
  canTake p = case getNumField p "size" of
    Just n  -> (\_ -> n < 3)
    Nothing -> (\_ -> False)
instance Eq Player where
  p1 == p2 = name p1 == name p2
instance Ord Player where
  p1 <= p2 = name p1 <= name p2
instance Show Player where
  show p = name p ++ ": " ++ desc p

-- | An object has a name, description, mapping from actions
-- | performed on it to strings to display to the Client,
-- | and the preconditions to uses of it.
data AdvObject = AdvObject String String String Req (Player -> GSTrans)
instance Thing AdvObject where
  name (AdvObject n _ _ _ _)   = n
  desc (AdvObject _ d _ _ _)   = d
  active _ = True
instance Takeable AdvObject where
  canTake _ _ = True
instance Usable AdvObject where
  isUsable (AdvObject _ _ _ r _) = r
  use (AdvObject _ _ _ _ u)      = u
  getActionStr (AdvObject _ _ uStr _ _) = uStr
instance Eq AdvObject where
  o1 == o2 = name o1 == name o2
instance Ord AdvObject where
  o1 <= o2 = name o1 <= name o2
instance Show AdvObject where
  show o = name o ++ ": " ++ desc o

-- | Can be used to model chests/bags.
data Bag = Bag String String [ThingBox]
instance Thing Bag where
  name (Bag n _ _) = n
  desc (Bag _ d _) = d
  active _ = True
instance Container Bag where
  contains (Bag _ _ c) = c
  add (Bag n d c) o = Bag n d ((TB o):c)
  remove (Bag n d c) o = Bag n d (L.delete (TB o) c)
instance Eq Bag where
  b1 == b2 = name b1 == name b2
instance Ord Bag where
  b1 <= b2 = name b1 <= name b2
instance Show Bag where
  show b = name b ++ ": " ++ (show $ fmap (\(TB t) -> show t) (contains b))
