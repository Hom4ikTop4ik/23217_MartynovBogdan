import Data.List (sort, nub) -- task 1: I need sort(nub list) for optimize tree
import Data.Maybe (catMaybes) -- task 4

data BinTree a =
    Nil |
    Node {
        left :: BinTree a,
        right :: BinTree a,
        value :: a,
        count :: Int
    }


-- для более удобного вывода
instance Show a => Show (BinTree a) where
    show = show0 0 where
        show0 _ Nil = "Nil"
        show0 lvl Node{left=l, right=r, value=v, count=cnt} =
            "Node (v = " ++ show v ++ ") (cnt = " ++ show cnt ++ ")\n" ++
            replicate lvl '\t' ++ "l=" ++ show0 (lvl+1) l ++ "\n" ++
            replicate lvl '\t' ++ "r=" ++ show0 (lvl+1) r ++ "\n"
            -- replicate lvl '\t' — печатает lvl раз символ '\t'
instance Eq a => Eq (BinTree a) where
    (==) tree1 tree2 = case (tree1, tree2) of
        (Nil, Nil) -> True
        _ -> False

testTree :: BinTree Int
testTree = Node {
    left = Node {
        left = Node {left = Nil, right = Nil, value = 1, count = 1},
        right = Node {
            left = Node {
                left = Nil, right = Nil, value = 4, count = 1},
            right = Node {
                left = Nil, right = Nil, value = 7, count = 1},
            value = 6, count = 1
        },
        value = 3, count = 1
    },
    right = Node {
        left = Nil,
        right = Node {
            left = Node {
                left = Nil, right = Nil, value = 13, count = 1},
            right = Nil,
            value = 14, count = 1
        },
        value = 10, count = 1
    },
    value = 8, count = 1
}

---------
-- task 1

insert :: Ord a => BinTree a -> a -> BinTree a
insert tree new_value | tree == Nil    = Node{left=Nil, right=Nil, value=new_value, count=1}
                      | new_value == v = Node{left=l, right=r, value=v, count=cnt+1}
                      | new_value < v  = Node{left=(insert l new_value), right=r, value=v, count=cnt}
                      | new_value > v  = Node{left=l, right=(insert r new_value), value=v, count=cnt} 
                      where Node{left=l, right=r, value=v, count=cnt} = tree

-- for unoptimized tree (very easy)
fromList :: Ord a => [a] -> BinTree a
fromList list = foldl (\acc x -> insert acc x) (insert Nil (head list)) (tail list) 

-- for optimized tree (harder)
-- fromList' :: Ord a => BinTree a -> [a] -> BinTree a
-- fromList' acc' x' = foldl (\acc x -> insert acc x) acc' x' -- i know eta reduce, but it's uncomfortable to watch

-- fromList :: Ord a => [a] -> BinTree a
-- fromList list = fromList' (insert Nil (head list)) (tail list) 


-- optimizedTree ::  Ord a => [a] -> BinTree a
-- optimizedTree list =    let nubbed_sorted_list = sort(nub list) in -- nub list removes repetitions
--                         let median list =  nubbed_sorted_list !! ((length nubbed_sorted_list) `div` 2) in
--                         let Node{left=l, right=r, value=v, count=cnt} = fromList' (insert Nil (median nubbed_sorted_list)) (list) in
--                         Node{left=l, right=r, value=v, count=cnt-1}
                    
                    

---------
-- task 2

findMin :: Ord a => BinTree a -> Maybe a
findMin tree    | tree == Nil = Nothing
                | l == Nil    = Just v
                | otherwise   = findMin l
                where Node{left=l, right=r, value=v, count=cnt} = tree

findMax :: Ord a => BinTree a -> Maybe a
findMax tree    | tree == Nil = Nothing
                | r == Nil    = Just v
                | otherwise   = findMax r
                where Node{left=l, right=r, value=v, count=cnt} = tree

---------
-- task 3
treeSort :: Ord a => BinTree a -> [a]
treeSort Nil = []
treeSort tree = (treeSort l) ++ (repeat' tree) ++ (treeSort r) 
                where Node{left=l, right=r, value=v, count=cnt} = tree

repeat' ::  Ord a => BinTree a -> [a]
repeat' tree  = let Node{left=l, right=r, value=v, count=cnt} = tree in 
                fst(foldl (\(acc, val) x -> (val:acc, val)) ([], v) [1..cnt]) 

---------
-- task4
findAny :: Ord a => (a -> Bool) -> BinTree a -> Maybe a
findAny pred tree   | tree == Nil = Nothing
                    | pred v = Just v
                    | otherwise = if (x == []) then Nothing else Just (head x)
                    where Node{left=l, right=r, value=v, count=cnt} = tree; x = (catMaybes ([findAny pred l]++[findAny pred r]))

---------
-- task*

-- first example for task*:
--         5
--      7     5
--     3 1   9 4
notSearchTree :: BinTree Int
notSearchTree = Node {
    left = Node {
        left = Node{left=Nil, right=Nil, value=3, count=1}, 
        right = Node{left=Nil, right=Nil, value=1, count=1}, 
        value = 7, count = 1
        },
    right = Node {
        left = Node {
            left = Nil, right = Nil, value = 9, count = 1},
        right = Node {
            left = Nil, right = Nil, value = 4, count = 1},
        value = 5, count = 1
    },
    value = 5, count = 1
}

-- second example for task*:
--         5
--      4     7
--     3 _   6 10
searchTree :: BinTree Int
searchTree = Node {
    left = Node {
        left = Node{left=Nil, right=Nil, value=3, count=1}, 
        right = Nil,
        value = 4, count = 1
        },
    right = Node {
        left = Node {
            left = Nil, right = Nil, value = 6, count = 1},
        right = Node {
            left = Nil, right = Nil, value = 10, count = 1},
        value = 7, count = 1
    },
    value = 5, count = 1
}



isSearchTree :: Ord a => BinTree a -> Bool
isSearchTree tree = helper_isSearchTree tree True

helper_isSearchTree tree acc = if (pred1 && pred2) then acc else False 
                            where pred1 = if(l /= Nil) then vl < v else True; 
                                  pred2 = if(r /= Nil) then v < vr else True; 
                                  Node{left=l, right=r, value=v, count=cnt} = tree; 
                                  Node{left=ll, right=rl, value=vl, count=cntl} = l; 
                                  Node{left=lr, right=rr, value=vr, count=cntr} = r

---------   
