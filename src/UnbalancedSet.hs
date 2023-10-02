{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, InstanceSigs #-}

module UnbalancedSet (UnbalancedSet, complete) where

import Set

data UnbalancedSet a = E | T (UnbalancedSet a) a (UnbalancedSet a) deriving Show

instance Ord a => Set UnbalancedSet a where
  empty = E

  member :: a -> UnbalancedSet a -> Bool
  member _ E = False
  member x (T a y b)
    | x < y = member' x Nothing a
    | x > y = member' x (Just y) b
    | otherwise = True


  insert :: a -> UnbalancedSet a -> UnbalancedSet a
  insert x E = T E x E
  insert x s@(T a y b)
    | x < y = T (insert' x E a) y b
    | x > y = T a y (insert' x s b)
    | otherwise = s

member' :: (Ord a) => a -> Maybe a -> UnbalancedSet a -> Bool
member' _ Nothing E = False
member' x (Just l) E = x == l
member' x l (T a y b)
  | x < y = member' x l a
  | otherwise = member' x (Just y) b

insert' :: Ord a => a -> UnbalancedSet a -> UnbalancedSet a -> UnbalancedSet a
insert' x E E = T E x E
insert' x t@(T _ y b) E
  | x == y = t
  | otherwise = insert' x E b
insert' x l s@(T a y b)
  | x < y = T (insert' x l a) y b
  | otherwise = T a y (insert' x s b)

complete :: a -> Int -> UnbalancedSet a
complete _ n | n <= 0 = E
complete x 1 = T E x E
complete x n =
  let sub = complete x (n-1)
  in T sub x sub
