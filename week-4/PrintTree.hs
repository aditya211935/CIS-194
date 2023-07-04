module PrintTree where

data Tree a = Leaf
            | Node Int (Tree a) a (Tree a)
  deriving (Show, Eq)

{-
1. Build left and right tree
2. Draw left connector and append to left array
3. Do same for right array
4. Combine both of them line by line with a space in the middle
-}

printTree :: Show a => Tree a -> [String]
printTree Leaf = [" "]
printTree (Node _ l a r) = node ++ (combine lt rt (length $ show a))
  where
    (ll, la, lr) = getWidth l
    (rl, ra, rr) = getWidth r
    ls = printTree l
    rs = printTree r
    -- Uncommenting this will allow subtrees to be of same height visaully. Bit buggy as connectors will be drawn even
    -- there's no node.
    -- conn = max (lr + la `div` 2) (rl + ra `div` 2)
    -- off = max (ll + la + lr) (rl + ra + rr)
    -- node = drawNode off off a
    -- cl = leftConnector off conn
    -- cr = rightConnector off conn
    node = drawNode (ll + la + lr) (rl + ra + rr) a
    cl = leftConnector (ll + la + lr) (lr + la `div` 2)
    cr = rightConnector (rl + ra + rr) (rl + ra `div` 2)
    lt = cl ++ ls
    rt = cr ++ rs

getTotal :: (Int, Int, Int) -> Int
getTotal (a, b, c) = a + b + c

getWidth :: Show a => Tree a -> (Int, Int, Int)
getWidth Leaf = (0, 1, 0)
getWidth (Node _ l val r) = (getTotal $ getWidth l, length $ show val, getTotal $ getWidth r)

combine :: [String] -> [String] -> Int -> [String]
combine [] a _ = a
combine a [] _ = a
combine a b off = map (\(x, y) -> x ++ (drawWhitespace off) ++ y) (zip a b ++ rest)
  where
    la = length a
    lb = length b
    offa = drawWhitespace (length (a !! 0))
    offb = drawWhitespace (length (b !! 0))
    rest = case compare la lb of
      GT -> map (\x -> (x, offb)) (drop lb a)
      LT -> map (\x -> (offa, x)) (drop la b)
      EQ -> []

drawWhitespace :: Int -> String
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

-- main :: IO()
-- main = do
  -- print (flip2 f 2 1)
  -- putStr (unlines $ printTree $ foldTree [0..7])
  -- putStr (unlines (printTree $ Node 1 (Node 1 Leaf 'b' Leaf) 'a' (Node 1 Leaf 'c' Leaf)))
  -- print ([drawWhitespace 3 ++ show 1 ++ drawWhitespace 4] )
  -- print ( (printTree $ Node 1 (Node 1 Leaf 'a' Leaf) 'b' (Node 1 (Node 1 (Node 1 Leaf 'e' Leaf) 'd' Leaf) 'c' Leaf)))
  -- print (drawNode 3 2 'a')
  -- print (leftConnector 1 0)
  -- print (drawNode 5 3 3)
  -- print (combine ["123"] ["12345", "12345"])
  -- putStr (unlines $ printTree $ foldTree "ABCDEFGHIJKLMNOPQRWXYZ")
  -- putStr (unlines $ printTree $ foldTree ["lola", "cola", "pola", "pola", "pola", "pola", "pola", "pola", "pola", "pola", "pola", "pola", "pola", "pola", "pola"])
  -- print (foldTree "ABCDEFGHIJ")
  -- let x = 420
  -- print (fun21 x)
  -- print (fun2 x)


-- 10, 5, 16, 8, 4, 2, 1

