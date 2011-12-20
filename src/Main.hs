{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where
import Engine
import Builder (sampleGS, mkPlayer)

main :: IO ()
main = do startEngine sampleGS mainPlayer where
            mainPlayer = mkPlayer pName pDesc True
            pName = "Main character"
            pDesc = "Some guy stuck in this maze."