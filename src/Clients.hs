{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Clients (annoyingNPCThread, main) where
import Builder (mkPlayer)
import Types2 (Player, Thing(name))
import Network (withSocketsDo, connectTo, PortID(..))
import System.IO (hPutStrLn, Handle)
import Control.Concurrent (forkIO, threadDelay)

-- | Contains definitions for NPC clients.
main :: IO ()
main = withSocketsDo $ do
       h <- connectTo "localhost" $ PortNumber 8080
       _ <- forkIO $ annoyingNPCThread gnome "yell Kids these days!" h
       return ()

gnome :: Player
gnome = mkPlayer npcName npcDesc False where
  npcName = "grumpy gnome"
  npcDesc = "This gnome comes with a permanent frown."

-- | Periodically yells nonsense to localhost.
annoyingNPCThread :: Player -> String -> Handle -> IO ()
annoyingNPCThread p s h = do _ <- hPutStrLn h ("addme " ++ name p) 
                             loop where
                               loop = do _ <- threadDelay 5000
                                         _ <- hPutStrLn h s
                                         loop