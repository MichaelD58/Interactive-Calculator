module BTTesting where

import BinarySearchTree
import Test.QuickCheck
import Data.Char
import System.Random

-- Generates a sample tree with no sub-nodes. Used for basic, 'by eye' checking. 
sample_tree :: Num a => BiTree String a
sample_tree = Node {left = Leaf, var = ("a", 0), right = Leaf}

-- Generates a sample tree with one, left-hand, sub-tree. Used for basic, 'by eye' checking. 
sample_left :: (Num a, Eq a) => BiTree String a
sample_left  = insert (Node {left = Leaf, var = ("b", 1), right = Leaf}) ("a", 0)

-- Generates a sample tree with multiple layers of branches. Used for basic, 'by eye' checking. 
sample_complex :: (Num a, Eq a) => BiTree String a
sample_complex = let root = Leaf in 
    let t1 = insert root ("m", 1) in 
        let t2 = insert t1 ("g", 10) in 
            let t3 = insert t2 ("z", 46) in 
                let t4 = insert t3 ("a", 1) in 
                    let t5 = insert t4 ("b", -9) in 
                        let t6 = insert t5 ("z", 0) in 
                            insert t6 ("r", -32)

-- |The 'ordered' function returns true if: 
-- |    1) A node's name is greater than the name of its left-hand sub-tree.
-- |    2) A node's name is less than the value of the name of its right-hand sub-tree.
-- |    3) The above is true for both sub-trees.
ordered :: (Num a, Eq a) => BiTree String a -- ^ The tree to be tested
                            -> Bool
ordered Leaf = True
ordered tree = 
    (case (left tree) == Leaf of 
    True -> True
    False -> fst (var tree) > fst (var (left tree)) &&
        ordered (left tree))
    &&
    (case (right tree) == Leaf of 
    True -> True
    False -> fst (var tree) < fst (var (right tree)) &&
        ordered (right tree))

-- Returns true if a given tree is ordered. 
prop_ordered :: (Num a, Eq a) => BiTree String a -> Bool
prop_ordered tree = ordered tree

-- An instance of the Arbitrary class to allow QuickCheck to generate random trees for testing. 
instance (Arbitrary a, Ord a, Arbitrary b, Eq b) => Arbitrary (BiTree a b) where 
    arbitrary = 
        sized arbitrary_sized_tree

-- Constructs a tree of arbitrary size by inserting arbitrarily generated (key, value) pairs into 
-- a tree recursively built up from a Leaf. 
arbitrary_sized_tree :: (Arbitrary a, Ord a, Arbitrary b, Eq b) => Int -> Gen (BiTree a b)
arbitrary_sized_tree m = do 
    val <- arbitrary
    name <- arbitrary
    n <- choose (0, m `div` 2)
    case n > 1 of 
        True -> do 
            root <- arbitrary_sized_tree n
            return (insert root (name, val))
        False -> do 
            return Leaf
