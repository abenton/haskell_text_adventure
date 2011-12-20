module Engine where
import Types2
import Control.Concurrent.MVar
import Data.Set as Set

-- | The basic idea behind the game engine is that there is a single game
-- | state which is being mutated by clients interacting through the engine.
-- | The engine keeps a mutable list of mappings from Players to command
-- | strings.  Clients concurrently add commands for the engine to execute
-- | along with the Player they correspond to.  These commands are parsed,
-- | executed if valid, and the proper object will be sent back for the
-- | client to display as it pleases.
-- |
-- | Clients may either be normal players sending messages to the engine via
-- | command line, or over the network.  They can also be AIs attached to NPCs.
-- | As far as the engine is concerned, there is no difference between PCs and
-- | NPCs.  In this demo the engine only listen on localhost for clients.

data Engine = Engine GS (MVar [(BasicClient, Action)])

class Client a where
  pName      :: a              -> String
  sendRoom   :: a ->      Room -> IO () 
  sendObj    :: (Objectable b) => a -> b -> IO ()
  sendMap    :: a -> Set Room  -> IO ()
  sendString :: a -> String    -> IO ()

data BasicClient = PC String
                   | NPC String
instance Client BasicClient where
  pName (PC  n) = n
  pName (NPC n) = n
  sendRoom client r = putStr "Got a room!"
  sendObj  client o = putStr "Got an object!"
  sendMap  client rooms = putStr "Got the whole map!"
  sendString client msg = putStr msg