{-# LANGUAGE InstanceSigs #-}

-- Block 1.5. Made separate module to avoid orphan instance Foldable Bst
module Block1_5
  ( Bst(..)
  , fromList
  , isEmpty
  , size
  , exists
  , insert
  , delete
  ) where

import Data.List.NonEmpty as NE hiding (fromList, insert)

-- Block 1. Task 3 (BST):
data Bst a
  = Leaf
  | TreeNode
      { equalKeys  :: NonEmpty a
      , leftChild  :: Bst a
      , rightChild :: Bst a
      }

-- | Checks that tree contains any elements.
isEmpty :: Bst a -> Bool
isEmpty Leaf = True
isEmpty _    = False

-- | Returns size of tree.
size :: Bst a -> Int
size Leaf                       = 0
size (TreeNode keys left right) = size left + size right + NE.length keys

-- | Checks that tree contains given element.
exists :: Ord a => Bst a -> a -> Bool
exists Leaf _ = False
exists (TreeNode keys left right) x =
  case compare x (NE.head keys) of
    LT -> exists left x
    GT -> exists right x
    EQ -> True

-- | Inserts new element in tree. If tree contains equal element,
-- they both stored as list in single node.
insert :: Ord a => Bst a -> a -> Bst a
insert Leaf x = TreeNode (x :| []) Leaf Leaf
insert (TreeNode keys left right) x =
  case compare x (NE.head keys) of
    LT -> TreeNode keys (insert left x) right
    GT -> TreeNode keys left (insert right x)
    EQ -> TreeNode (x :| toList keys) left right

-- | Returns tree made from list elements
fromList :: Ord a => [a] -> Bst a
fromList = foldl insert Leaf 

-- | Delete given element from tree.
delete :: Ord a => Bst a -> a -> Bst a
delete Leaf _ = Leaf
delete (TreeNode keys left right) x =
  case compare x (NE.head keys) of
    LT -> TreeNode keys (delete left x) right
    GT -> TreeNode keys left (delete right x)
    EQ -> delete' keys left right
      where delete' :: NonEmpty a -> Bst a -> Bst a -> Bst a
            delete' (_ :| y : xs) l r = TreeNode (y :| xs) l r
            delete' _ Leaf Leaf = Leaf
            delete' _ Leaf r = r
            delete' _ l Leaf = l
            delete' (_ :| _) l r =
              let successor = minNode r
               in TreeNode (equalKeys successor) l (rightChild successor)

-- | Returns node with minimal element (leftmost node).
minNode :: Bst a -> Bst a
minNode tree =
  case tree of
    (TreeNode _ Leaf _) -> tree
    (TreeNode _ left _) -> minNode left
    _                   -> tree

-- Block 2. Task 1 (instance Foldable for Tree)
instance Foldable Bst where
  foldMap :: Monoid m => (a -> m) -> Bst a -> m
  foldMap _ Leaf = mempty
  foldMap f (TreeNode keys left right) =
    case keys of
      (x :| []) -> (foldMap f left <> f x) <> foldMap f right
      (x :| y : xs) ->
        (foldMap f left <> f x) <> foldMap f (TreeNode (y :| xs) Leaf right)
  foldr :: (a -> b -> b) -> b -> Bst a -> b
  foldr _ z Leaf = z
  foldr f z (TreeNode keys left right) =
    case keys of
      (x :| []) -> foldr f (f x (foldr f z right)) left
      (x :| y : xs) ->
        foldr f (f x (foldr f z right)) (TreeNode (y :| xs) left Leaf)
