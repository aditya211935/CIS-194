{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module PrintTree2 where

data Tree a = Leaf
            | Node Int (Tree a) a (Tree a)
  deriving (Show, Eq)

{-
COMPACT VERSION:
This version doesn't give Leaf a whitespace, thus resulting in a compact form.
However, the logic needed to do this is kindof messy.
In hindsight, we could probably get rid of getWidth from the other version too, and compute the offsets
directly from comparing whitespaces to the left and right of the node.

This is because the width of the tree doesn't necessarily tell us about how many connectors will be reuired or what its offset will be.
Case in point: For a tree a with b & c children, the width of b is 1, thus its offset will be 1.
Its connectors is dependent on b's right subtree's width which is zero. However correct value of connectors and offset is
1 and 2 respectively.

1. Build left and right tree
2. Left connecter is min (size of left's right subtree, 1).
3. If size is 1, need to pad the left subtree (special case)
3. Draw left connector and append to left array
4. Do same for right array
5. Combine both of them line by line with a space in the middle
-}

printTree :: Show a => Tree a -> [String]
printTree Leaf = [""]
printTree (Node _ l a r) = node ++ combine lt rt (length $ show a)
  where
    ls = addPaddingLeft l $ printTree l
    rs = addPaddingRight r $ printTree r
    lOff = length (ls !! 0)
    rOff = length (rs !! 0)
    node = drawNode lOff rOff a
    cl = leftConnector lOff (rightSpace (head ls) + elemSpace (head ls) `div` 2)
    cr = rightConnector rOff (leftSpace (head rs) + elemSpace (head rs) `div` 2)
    lt = cl ++ ls
    rt = cr ++ rs

addPaddingLeft :: Tree a -> [String] -> [String]
addPaddingLeft Leaf val = val
addPaddingLeft (Node _ _ _ Leaf) val = map (\x -> x ++ [' ']) val
addPaddingLeft _ val = val

addPaddingRight :: Tree a -> [String] -> [String]
addPaddingRight Leaf val = val
addPaddingRight (Node _ Leaf _ _) val = map (\x -> [' '] ++ x) val
addPaddingRight _ val = val

combine :: [String] -> [String] -> Int -> [String]
combine [] a _ = a
combine a [] _ = a
combine a b off = map (\(x, y) -> x ++ (drawWhitespace off) ++ y) (zip a b ++ rest)
  where
    la = length a
    lb = length b
    offa = drawWhitespace (length (head a))
    offb = drawWhitespace (length (head b))
    rest = case compare la lb of
      GT -> map (\x -> (x, offb)) (drop lb a)
      LT -> map (\x -> (offa, x)) (drop la b)
      EQ -> []

drawWhitespace :: Int -> String
-- drawWhitespace l = intercalate "" $ map (\x -> show x) [1..l]
drawWhitespace l = replicate l ' '

leftConnector :: Int -> Int -> [String]
leftConnector off len = [drawSingle x|x <- [0..len-1]]
  where
    drawSingle i = drawWhitespace (off - i - 1) ++ ['/'] ++ drawWhitespace i

rightConnector :: Int -> Int -> [String]
rightConnector off len = [drawSingle x|x <- [0..len-1]]
  where
    drawSingle i = drawWhitespace i ++ ['\\'] ++ drawWhitespace (off - i - 1)

drawNode :: Show a => Int -> Int -> a -> [String]
drawNode lOff rOff a = [drawWhitespace lOff ++ show a ++ drawWhitespace rOff]

-- Gives the length of space taken by element itself.
elemSpace :: String -> Int
elemSpace s = length s - (leftSpace s + rightSpace s)

leftSpace :: String -> Int
leftSpace = length . takeWhile (==' ')

rightSpace :: String -> Int
rightSpace = length . takeWhile (==' ') . reverse

-- main :: IO()
-- main = do
  -- print (flip2 f 2 1)
  -- putStr (unlines (printTree $ Node 1 (Node 1 Leaf 0 Leaf) 0 Leaf))
  -- print ([drawWhitespace 3 ++ show 1 ++ drawWhitespace 4] )
  -- putStr (unlines (printTree $ Node 1 (Node 1 Leaf 1 Leaf) 2 (Node 1 (Node 1 (Node 1 Leaf 3 Leaf) 4 Leaf) 5 Leaf)))
  -- putStr (unlines (printTree $ Node 1 (Node 1 Leaf 0 Leaf) 0 (Node 1 (Node 1 Leaf 0 Leaf) 0 Leaf)))
  -- print (drawNode 3 2 'a')
  -- print (leftConnector 1 0)
  -- print (drawNode 5 3 3)
  -- print (combine ["123"] ["12345", "12345"])
  -- putStr (unlines $ printTree $ foldTree "ABCDEFGHIJKLMNOPQRWXYZ")
  -- putStr (unlines $ printTree $ foldTree ([0..7] :: [Int]))
  -- putStr (unlines $ printTree $ foldTree ["lola", "cola", "pola", "pola", "pola", "pola", "pola", "pola", "pola", "pola", "pola", "pola", "pola", "pola", "pola"])
  -- print (foldTree "ABCDEFGHIJ")
  -- let x = 420
  -- print (fun21 x)
  -- print (fun2 x)


-- 10, 5, 16, 8, 4, 2, 1

