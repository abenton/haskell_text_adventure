{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell, ExistentialQuantification #-}

module Types2 (
    -- Exported data types
    Dir(..),
    Action(..),
    Door(..),
    GS(..),
    Room(..),
    AdvObject(..),
    Bag(..),
    Player(..),
    Thing(..),
    Usable(..),
    Takeable(..),
    Container(..),
    Objectable,
    ThingBox(..),
    GSTrans,
    DispResp,
    Req,
    Sudo
) where 

import Data.List as L
import Data.Set as Set
import Data.Map as Map

-- | Possible actions that a player can issue.
data Action = Go Dir
              | Get   AdvObject
              | Drop  AdvObject
              | Use   AdvObject
              | Say   String
              | Yell  String
              | Look
              | LookAt String
              | SeeAll
              | MkObj AdvObject
              | MkRm  Door Room
              | MkBag Bag
              | MkPlayer Player
              | RmObj String
              | RmDoor Dir
              | SetStat String String Int
              | MustHaveStatLE String String Int
              | MustHaveStatGE String String Int
              | MustHaveNObjs String String Int
              | Teleports Room String
              | SetsStat String String Int
              | Inv
              | Stats
              | AddMe String
              | Help
              | Save
              | Quit deriving (Show, Eq, Ord)

-- | 12/17/11 AB: May expand this later.  Thought it good to keep a finite set
-- | of directions to start with.
data Dir = N | S | E | W | U | D deriving (Show, Eq, Ord)

-- | 12/17/11 AB: A door is a generic term for an exit.  Takes a short 
-- | description/name, direction, and a precondition for opening as arguments.
data Door = Door String Dir Req
instance Eq Door where
  (Door _ d1 _) == (Door _ d2 _) = d1 == d2
instance Ord Door where
  (Door _ d1 _) <= (Door _ d2 _) = d1 <= d2
instance Show Door where
  show (Door n _ _) = n

-- | 12/17/11 AB: Represents a precondition for performing an action.
-- | Dependent solely on the state of the Player.
type Req = Player -> Bool

-- | 12/17/11 AB: The current game state.  A map is just a set of rooms.
-- | Each room contains a list of child objects.  The mapping from clients
-- | controlling players to the player objects is maintained by the engine.
data GS = GS (Set Room)

-- | Game state transformer
type GSTrans = GS -> GS

-- | 12/17/11 AB: Things encompass just about everything in the game world.
-- | Rooms, objects, PCs, NPCs.  The flexibility that Thing provides allows
-- | interesting game play, like picking up a gnome NPC and dropping it in
-- | your bag.  Also allows any object in the game to have a client attached
-- | to it, that can issue actions to the game engine.
class (Eq a, Show a) => Thing a where
  -- | All objects must have a name.  This name is used to test for equality.
  name :: a -> String
  setName :: String -> a -> a
  
  desc :: a -> String
  desc _ = ""
  setDesc :: String -> a -> a
  
  -- | When Things are inactive, then they are unable to interact with the
  -- | game world.
  inactive :: a -> Bool
  inactive = not . active
  active   :: a -> Bool
  active   = not . inactive
  
  getNumField :: a -> String -> Maybe Int
  getNumField _ _ = Nothing
  getNumFields :: a -> Stats
  getNumFields _  = Map.empty
  setNumField :: String -> Int -> a -> a
  setNumField _ _ a = a
  getStrField :: a -> String -> Maybe String
  getStrField t "name" = Just $ name t
  getStrField t "desc" = Just $ desc t
  getStrField _ _ = Nothing
  setStrField :: String -> String -> a -> a
  setStrField "name" n a = setName n a
  setStrField "desc" d a = setDesc d a
  setStrField _ _ a = a

-- | Wrapper around Thing types.  This is to allow containers to have
-- | bags, players, or objects as children.
data ThingBox = forall t. Objectable t => TB t
instance Eq ThingBox where
  (TB a) == (TB b) = name a == name b
instance Show ThingBox where  
  show (TB a) = show a

class (Thing a) => Container a where
  contains :: a -> [ThingBox]
  add      :: (Objectable b) => a -> b -> a
  remove   :: (Objectable b) => a -> b -> a
  replace  :: (Objectable b, Objectable c) => a -> b -> c -> a
  replace a b c = add (remove a b) c
  move     :: (Container b, Objectable c) => a -> c -> b -> (a, b)
  move src t dst = (remove src t, add dst t)
  moveAll  :: (Container b) => a -> b -> (a, b)
  moveAll src dst = foldr moveFn (src,dst) (contains src) where
    moveFn (TB a) (s', d') = move s' a d'
  (==>)    :: (Container b) => a -> b -> (a, b)
  src ==> dst = moveAll src dst

-- | Implemented by objects that can be put into inventory.
class (Thing a) => Takeable a where
  canTake :: a -> Req
  canTake _ _ = True

-- | String response to performing an action.
-- type DispResp = Action -> String

class (Thing a) => Usable a where
  isUsable     :: a -> Req
  isUsable  _ _ = True
  setUsable    :: a -> Req -> a
  setUsable a _ = a
  use          :: a -> Player -> GSTrans
  use       _ _ = id
  setEffect    :: a -> (Player -> GSTrans) -> a
  setEffect a _ = a
  getActionStr :: a -> String
  getActionStr a = "Something happened because of " ++ (name a) ++ "."

class (Container a, Takeable a, Usable a) => Objectable a

-- | 12/17/11 AB: A room.  Pass it a unique name, description, mapping from
-- | doors to other rooms, and a list of children.
data Room = Room String String (Map Door Room) [ThingBox]
instance Thing Room where
  name (Room n _ _ _)     = n
  setName n' (Room _ d m c) = Room n' d m c
  desc (Room _ d _ _)     = d
  setDesc d' (Room n _ m c) = Room n d' m c
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

-- | The response that should be sent back to the client to be displayed.
type DispResp = Action -> String

-- | Statistics for a character, can be set arbitrarily.
type Stats = Map String Int

-- | Flags this player as a super-user.  Has access to additional commands.
type Sudo = Bool

-- | Player: has a name, description, stats, inventory, whether they are
-- | active, and are they a super user.  PC indistinguishable from an NPC.
-- | Only difference is the Client controlling it.
data Player = Player String String Stats [ThingBox] Bool Sudo
instance Thing Player where
  name (Player n _ _ _ _ _) = n
  setName n' (Player _ d s i a su) = Player n' d s i a su
  desc (Player _ d _ _ _ _) = d
  setDesc d' (Player n _ s i a su) = Player n d' s i a su
  active (Player _ _ _ _ a _) = a
  getNumField (Player _ _ s _ _ _) k = Map.lookup k s
  getNumFields (Player _ _ s _ _ _) = s
  setNumField k v (Player n d s i a su) = Player n d (Map.insert k v s) i a su
instance Container Player where
  contains (Player _ _ _ c _ _) = c
  add (Player n d s c a su) o = Player n d s ((TB o):c) a su
  remove (Player n d s c a su) o = Player n d s (L.delete (TB o) c) a su
instance Takeable Player where
  canTake p = case getNumField p "size" of
    Just n  -> (\_ -> n < 3)
    Nothing -> (\_ -> False)
instance Usable Player where
  isUsable _ _   = False
  getActionStr _ = "It's not right to use someone else."
instance Objectable Player
instance Eq Player where
  p1 == p2 = name p1 == name p2
instance Ord Player where
  p1 <= p2 = name p1 <= name p2
instance Show Player where
  show p = name p ++ ": " ++ desc p

-- | An object has a name, description, use string, preconditions for using
-- | it, and use effect.
data AdvObject = AdvObject String String String Req (Player -> GSTrans)
instance Thing AdvObject where
  name (AdvObject n _ _ _ _)   = n
  setName n' (AdvObject _ d uStr req trans) = AdvObject n' d uStr req trans
  desc (AdvObject _ d _ _ _)   = d
  setDesc d' (AdvObject n _ uStr req trans) = AdvObject n d' uStr req trans
  active _ = True
instance Container AdvObject where
  contains _ = []
  add o _    = o
  remove o _ = o
instance Takeable AdvObject where
  canTake _ _ = True
instance Usable AdvObject where
  isUsable (AdvObject _ _ _ r _) = r
  setUsable (AdvObject n d resp _ u) req = AdvObject n d resp req u
  use (AdvObject _ _ _ _ u)      = u
  setEffect (AdvObject n d resp req _) u = AdvObject n d resp req u
  getActionStr (AdvObject _ _ uStr _ _) = uStr
instance Objectable AdvObject
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
  setName n' (Bag _ d i) = Bag n' d i
  desc (Bag _ d _) = d
  setDesc d' (Bag n _ i) = Bag n d' i
  active _ = True
instance Container Bag where
  contains (Bag _ _ c) = c
  add (Bag n d c) o = Bag n d ((TB o):c)
  remove (Bag n d c) o = Bag n d (L.delete (TB o) c)
instance Takeable Bag where
  canTake _ _ = False
instance Eq Bag where
  b1 == b2 = name b1 == name b2
instance Ord Bag where
  b1 <= b2 = name b1 <= name b2
instance Show Bag where
  show b = name b
