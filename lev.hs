{-
  Levenstein distance
-}
shrinkStr s = take (length s - 1) s

frk :: String -> String -> Int
frk x y = if drop 1 x == drop 1 y then 0 else 1

lev :: String -> String -> Int
lev "" s = length s
lev s "" = length s
lev x y  = min (min (lev (shrinkStr x) y + 1)
                    (lev (shrinkStr y) x + 1))
                    (lev (shrinkStr x) (shrinkStr y) + frk x y)

main = print (lev "cat" "dots")