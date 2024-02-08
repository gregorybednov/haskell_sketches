data Tern = Yes | Neu | No deriving (Enum)

s0 :: Float -> (Tern, [Float])
s0 0 = (Yes, [])
s0 c = (No, [])

s1 :: Float -> Float -> (Tern, [Float])
s1 0 c = s0 c
s1 b c = (Neu, [-c/b])

s2 :: Float -> Float -> Float -> (Tern, [Float])
s2 0 b c = s1 b c
s2 a b c
    | discr a b c < 0 = (No, [])
    | discr a b c == 0 = (Neu, [-b/(2*a)])
    | discr a b c > 0 = (Neu, [(-b-discr a b c)/(2*a), (-b+discr a b c)/(2*a)])

discr :: Float -> Float -> Float -> Float
discr a b c = a*a - 4*b*c

main :: IO ()
main = do
    print(show (snd (s2 0 2 0)))