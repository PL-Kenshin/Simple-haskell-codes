--FINISHED

--addDivide :: [Int] -> [Int]
addDivide = map (\ x -> (x+1)/2)

addDivide2 = map ((/2).(+1))

addDivide3 = map (/2) . map (+1)





main = do
    let list = [1,2,3,4,5]
    
    print(addDivide list)
    print(addDivide2 list)
    print(addDivide3 list)
