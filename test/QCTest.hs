import  qualified Test.QuickCheck

main = do
        Test.QuickCheck.quickCheck checkReverse
        Test.QuickCheck.quickCheck checkReverseConcat
        Test.QuickCheck.quickCheck checkTreeSize
        Test.QuickCheck.quickCheck checkTreeSizeAdd
        Test.QuickCheck.quickCheck checkTreeSwap

checkReverse :: [Int] -> Bool
checkReverse xs = (reverse.reverse) xs == xs

checkReverseConcat :: [Int] -> [Int] -> Bool
checkReverseConcat xs ys = reverse(xs ++ ys) == (reverse ys) ++ (reverse xs)

data Tree = Nil | Node Int Tree Tree deriving (Eq, Ord, Show, Read)

instance Test.QuickCheck.Arbitrary Tree where
       arbitrary = Test.QuickCheck.sized arbTree

arbTree :: Int -> Test.QuickCheck.Gen Tree
arbTree 0 = do
                return Nil
arbTree n = do
                (Test.QuickCheck.Positive m) <- Test.QuickCheck.arbitrary
                let n' = n `div` (m+1)
                (Test.QuickCheck.Positive a) <- Test.QuickCheck.arbitrary
                left <- arbTree n'
                right <- arbTree n'
                return (Node a left right)

treeSize :: Tree -> Int
treeSize Nil = 0
treeSize (Node x left right) = 1 + (treeSize left) + treeSize (right)

treeAdd :: Tree -> Int -> Tree
treeAdd Nil n = (Node n Nil Nil)
treeAdd (Node x left right) n
        | x >= n = (Node x (treeAdd left n) right)
        | x < n = (Node x left (treeAdd right n))

treeSwap :: Tree -> Tree
treeSwap Nil = Nil
treeSwap (Node x left right) = (Node x right left)

checkTreeSwap :: Tree -> Bool
checkTreeSwap tree = (treeSwap . treeSwap) tree == tree

checkTreeSize :: Tree -> Bool
checkTreeSize tree = (treeSize tree) == (treeSize (treeSwap tree))

checkTreeSizeAdd :: Tree -> Bool
checkTreeSizeAdd tree = (treeSize tree) + 1 == (treeSize (treeAdd tree 5))
