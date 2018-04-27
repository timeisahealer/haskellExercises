type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi x a b c  = hanoiRec x a b c

hanoiRec :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoiRec x a b c
  | x == 1 = [(a, c)]
  | otherwise = (hanoiRec) (x-1) a b c ++ [(a, b)] ++ (hanoiRec) (x-1) c a b
    
