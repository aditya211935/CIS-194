{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Employee
import Data.Tree
import Data.List ( sortOn )

glCons :: Employee -> GuestList -> GuestList
glCons e (GL list f) = GL (e:list) (f + empFun e)

moreFun :: GuestList -> GuestList -> GuestList
moreFun l r
  | l > r = l
  | otherwise = r

instance Semigroup GuestList where
  (<>) (GL lList lFun) (GL rList rFun) = GL (lList ++ rList) (lFun + rFun)
instance Monoid GuestList where
  mempty = GL [] 0

-- Tree can have arbitary number of children, so pass an array of b instead of l and r
treeFold :: b -> (a -> [b] -> b) -> Tree a -> b
-- When empty, treat children as Empty and supply initial value
treeFold z f (Node { subForest = [], rootLabel }) = f rootLabel [z]
treeFold z f node = f (rootLabel node) (map (treeFold z f) (subForest node))

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e [] = (GL [e] (empFun e), GL [] 0)
nextLevel e list = (with, without)
  where
    with = foldr (\c acc -> snd c <> acc) (GL [e] (empFun e)) list
    without = foldr (\(a, b) acc -> moreFun a b <> acc) (GL [] 0) list

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold (GL [] 0, GL [] 0) nextLevel

format :: GuestList -> IO ()
format (GL l f) = putStrLn ("Total fun: " ++ show f) >> mapM_ (putStrLn . empName) (sortOn empName l)

main :: IO ()
main = do
  -- print (maxFun testCompany)
  readFile "company.txt" >>= (\n -> format $ maxFun (read n :: Tree Employee))
