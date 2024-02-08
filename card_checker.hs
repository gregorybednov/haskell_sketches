import Data.Int

{-
Card number checker. task solved in FP
-}

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n = mod n 10 : toDigits (div n 10)

doubleEverySecEl :: [Integer] -> [Integer]
doubleEverySecEl [] = []
doubleEverySecEl [x] = [x]
doubleEverySecEl (x:y:cdr) = x : 2*y : doubleEverySecEl cdr

intlisToDigits :: [Integer] -> [Integer]
intlisToDigits [] = []
intlisToDigits [x] = toDigits x
intlisToDigits (x:cdr) = toDigits x ++ intlisToDigits cdr

n = 4012888888881882


validate :: Integer -> Bool
validate n = mod (sum (intlisToDigits (doubleEverySecEl (toDigits n)))) 10 == 0


main = print (show (validate n)  ++ "!")
