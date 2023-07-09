module Main where

import JoinList
import Editor

main = runEditor editor $ fromList ["a", "b", "c", "d", "e", "f", "g", "h"]
