module BinarySearchTree where

-- left is less than, right is greater than
-- sort by name because they are unique but values may not be 

-- | Binary trees made up of nodes and leaves. Values stored as tuples. 
data BiTree a b = Leaf 
            | Node {left :: BiTree a b, 
                    var :: (a , b), 
                    right :: BiTree a b } 
    deriving (Show, Eq)

-- | 'insert' will insert a (key, value) pair into the correct place in a tree. Duplicate names will overwrite the previous value. Does not guarantee balance. 
insertBST :: (Eq b, Eq a, Ord a) => BiTree a b -- ^ The tree to be inserted into. 
                              -> (a, b) -- ^ The (key, value) pair to be inserted. 
                              -> BiTree a b
insertBST Leaf v = Node Leaf v Leaf
insertBST node v = 
    case compare (fst v) (fst (var node)) of 
    LT -> if left node == Leaf then 
        let new_node = Node {left = Leaf, var = v, right = Leaf} in 
            Node {left = new_node, 
                var = var node, 
                right = right node} 
            else
            Node {left = insertBST (left node) v, 
                var = var node, 
                right = right node}
    EQ -> Node {left = left node, var = v, right = right node} 
        -- just updates the value in the case that the variable name already exists 
    GT -> if right node == Leaf then 
        let new_node = Node {left = Leaf, var = v, right = Leaf} in 
            Node {left = left node, 
                var = var node, 
                right = new_node} 
            else
            Node {left = left node, 
                var = var node, 
                right = insertBST (right node) v} 

-- | 'retrieve' will return the value associated with a key, or an error if no instance of the key is found. 
retrieve :: BiTree String a -- ^ The tree to be searched. 
         -> String -- ^ The key to be searched for. 
         -> Either String a
retrieve Leaf name = Left ("'" ++ name ++ "' " ++ "hasn't been declared") 
retrieve node name = 
    case compare name (fst (var node)) of 
       LT -> retrieve (left node) name 
       EQ -> Right (snd (var node)) 
       GT -> retrieve (right node) name 