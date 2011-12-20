{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where
import Engine
import Builder (sampleGS)
import Control.Concurrent.Chan (newChan)

main :: IO ()
main = do engine <- newEngine
          return ()

main :: IO ()
main = withSocketsDo $ do
                          socket <- listenOn $ PortNumber 8080
                          forkIO $ remoteHandler socket
                          forkIO $ localHandler

newEngine :: IO Engine
newEngine = fmap (Engine sampleGS) newChan