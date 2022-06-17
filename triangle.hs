--FINISHED

printSpace 0 = return ()
printSpace n = do
    putStr(" ")
    printSpace (n-1)

printSymbol 0 c = return ()
printSymbol n c = do
    putStr(c)
    printSymbol (n-1) c



printTree 0 c l= return ()
printTree n c l= do
    printSpace n
    printSymbol (l) c
    putStr("\n")
    printTree (n-1) c (l+2)
  

--main :: IO ()
main = do
    putStr("Podaj rozmiar drzewa:\n")
    height <- getLine
    let treeHeight = (read height :: Int)
    if treeHeight `mod` 2 == 0 then
        let char = "*" in 
        printTree treeHeight char 1
    else
        let char = "#" in
        printTree treeHeight char 1
        
    if treeHeight /= 7
        then main
    else
        return 0
    
