type Peg = String
type Move = (Peg, Peg)

{-
  Hanoi towers
-}

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 from to tmp = [(from, to)]
hanoi n from to tmp =
       hanoi (n-1) from tmp to
    ++ hanoi 1 from to tmp
    ++ hanoi (n-1) tmp to from


main = print (show (hanoi 2 "a" "b" "c"))