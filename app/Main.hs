module Main where

import Lib
import Graphics.Matplotlib

main :: IO ()
main = onscreen $ plot3d $ trajectory lorenzSystemWikipedia 0.008 3000 [State [2,1,1]]
