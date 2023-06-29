type Peg = String
type Move = (Peg, Peg)
hanoi :: Int -> Peg -> Peg -> Peg -> [Move]

hanoi 2 x y z = [(x, y), (x, z), (y, z)]
hanoi n x y z = hanoi (n-1) x z y ++ [(x, z)] ++ hanoi (n-1) y x z

main = print(hanoi 5 "a" "b" "c")
