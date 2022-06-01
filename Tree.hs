--FINISHED

data Tree a = EmptyNode 
              | Leaf a 
              | Node a (Tree a) (Tree a)

showTree :: Show a => Tree a -> Int -> String
showTree tree lvl =
    let space = replicate (3*lvl) ' ' in
    case tree of
    EmptyNode -> space ++ "()"
    (Node n tl tr) -> showTree tl (lvl+1) ++ "\n" ++ space ++ (show n) ++ "\n" ++ showTree tr (lvl+1)
    (Leaf n) -> space ++ (show n)
    
addElem :: Ord a => a -> Tree a -> Tree a
addElem a EmptyNode = Leaf a
addElem a (Node n tl tr) 
    | a < n = Node n (addElem a tl) tr 
    | a > n = Node n tl (addElem a tr)
    | otherwise = Node n tl tr
addElem a (Leaf n)
    | a < n = Node n (Leaf a) (EmptyNode) 
    | a > n = Node n (EmptyNode) (Leaf a)
    
tree2list :: Ord a => Tree a -> [a]
tree2list EmptyNode = [ ]
tree2list(Node n tl tr) = tree2list tl ++ [n] ++ tree2list tr
tree2list(Leaf n) = [n]

treeHeight :: Tree a -> Int
treeHeight (Leaf a) = 0
treeHeight (Node n tl tr) = 1 + treeHeight tl + treeHeight tr

treeLeafs :: Tree a -> Int
treeLeafs (Leaf a) = 1
treeLeafs (Node n tl tr) = treeLeafs tl + treeLeafs tr

list2tree :: [a] -> Tree a
list2tree [] = EmptyNode
list2tree [x] = Leaf x
list2tree list = Node x (list2tree ltx) (list2tree gtx)
                 where 
                 m = length list `div` 2
                 x = list !! m
                 ltx = take m list
                 gtx = drop (m+1) list



mkTree  = Node 5 (Node 3 (Leaf 4) (Leaf 5)) (Node 7 (Leaf 9) (Leaf 2)) 
main = do
    --putStrLn(showTree mkTree 0)
    let ls = [3,7,2,8,1,4,6,9,0,5]
    let ls2 = [1,2,3,4,5,6,7,8,9]
    --putStrLn(showTree (addElem 1 (Leaf 0)) 0)
    --print(tree2list mkTree)
    --print(treeHeight mkTree)
    --print(treeLeafs mkTree)
    putStrLn(showTree (list2tree ls) 0)
    
