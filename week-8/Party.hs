{-# OPTIONS_GHC -Wall #-}
module Main where

import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e (GL list f) = GL (e:list) (f + empFun e)

main :: IO ()
main = do
  print "lola"
