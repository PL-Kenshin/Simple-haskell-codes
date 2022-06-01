--FINISHED
avg :: [Int] -> Int
avg list = sum list `div` length list

avg2 :: [Int] -> Int
avg2 list = foldr(+) 0 list `div` length list

flatten :: [[Int]] -> [Int]
flatten list = foldl(++) [] list

multiply :: [Int] -> [Int]
multiply list = map (\x -> x * length list) list

multiply2 :: [Int] -> [Int]
multiply2 list = map (* length list) list

main = do
    let list1   =   [1,2,3]
    let list2   =   [4,5,6]
    let list3   =   [11,4,22,44]
    let ll      =   [list1, list2, list3]
    
    print(avg list1)
    print(avg2 list1)
    print(flatten ll)
    print(multiply list3)
    print(multiply2 list3)
