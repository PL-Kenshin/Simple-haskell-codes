--FINISHED

factorial :: Integer -> Integer
factorial x
    | x < 0 = -1
    | x > 1 = x * factorial (x - 1)
    | otherwise = 1

factorial2 :: Integer -> Integer
factorial2 x =
    if x < 0 then -1
    else if x > 1 then x * factorial (x - 1)
    else 1






main = do
    print(factorial (-2))
    print(factorial2 6)
