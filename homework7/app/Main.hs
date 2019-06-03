module Main where

import StringBuffer
import Editor
import Scrabble
import Sized
import JoinList
import Buffer

-- This version of main was provided with the homework in the
-- "StringBufEditor.hs" file.
-- main :: IO ()
-- main = runEditor editor $ unlines
--           [ "This buffer is for notes you don't want to save, and for"
--           , "evaluation of steam valve coefficients."
--           , "To load a different file, type the character L followed"
--           , "by the name of the file."
--           ]

-- Exercise 4
initialBuffer :: JoinList (Score, Size) String
initialBuffer = fromString (unlines
  [ "This buffer is for notes you don't want to save, and for"
  , "evaluation of steam valve coefficients."
  , "To load a different file, type the character L followed"
  , "by the name of the file."
  ])

main :: IO ()
main = runEditor editor initialBuffer
