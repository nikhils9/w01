module W0108 where

data Tree a = Node (Tree a) a (Tree a)
    | Leaf
    deriving (Show)

buildTree :: a -> Int -> Tree a
buildTree x h
    | h <= 0 = Leaf
    | otherwise = Node (buildTree x (h-1)) x (buildTree x (h-1))

labelTreeOrig :: Tree a -> Tree (Int, a)
labelTreeOrig t =
    fst $ labelTree'Orig t 0

labelTree'Orig :: Tree a -> Int -> (Tree (Int, a), Int)
labelTree'Orig Leaf currentLabel = (Leaf, currentLabel)
labelTree'Orig (Node l x r) currentLabel = 
    let
        (l', currentLabel') = labelTree'Orig l currentLabel
        labelForX = currentLabel'
        nextLabel = labelForX + 1
        (r', currentLabel'') = labelTree'Orig r nextLabel
    in 
        (Node l' (labelForX, x) r', currentLabel'')

labelTree :: Tree a -> Tree (Int, a)
labelTree t =
    fst $ runWithLabel (labelTree' t) 0

data WithLabel a = MkWithLabel { runWithLabel :: Int -> (a, Int)}

labelTree' :: Tree a -> WithLabel (Tree (Int, a))
labelTree' Leaf = returnWithLabel Leaf
labelTree' (Node l x r) = 
    labelTree' l    >>>= (\l' ->
    tick            >>>= (\labelForX ->
    labelTree' r    >>>= (\r' ->
        MkWithLabel (\currentLabel -> (Node l' (labelForX, x) r', currentLabel))))
    )

tick :: WithLabel Int
tick = MkWithLabel (\currentLabel -> (currentLabel, currentLabel + 1))

returnWithLabel :: a -> WithLabel a
returnWithLabel a = MkWithLabel (\currentLabel -> (a, currentLabel))

bindWithLabel :: WithLabel a -> (a -> WithLabel b) -> WithLabel b
bindWithLabel computation continuation =
    MkWithLabel (\currentLabel -> 
        let 
            (a, currentLabel') = runWithLabel computation currentLabel
        in
            runWithLabel (continuation a) currentLabel'
    )

(>>>=) = bindWithLabel