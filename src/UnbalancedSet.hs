{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, InstanceSigs #-}

module UnbalancedSet (UnbalancedSet) where

import Set

data UnbalancedSet a = E | T (UnbalancedSet a) a (UnbalancedSet a)

instance Ord a => Set UnbalancedSet a where
  empty = E

  member :: a -> UnbalancedSet a -> Bool
  member _ E = False
  member x (T a y b)
    | x < y = member' x Nothing a
    | x > y = member' x Nothing b
    | otherwise = True


  insert :: a -> UnbalancedSet a -> UnbalancedSet a
  insert x E = T E x E
  insert x s@(T a y b)
    | x < y = T (insert x a) y b
    | x > y = T a y (insert x b)
    | otherwise = s

member' :: (Ord a) => a -> Maybe a -> UnbalancedSet a -> Bool
member' _ Nothing _ =  False
member' x (Just l) E = x == l
member' x l (T a y b)
  | x < y = member' x l a
  | x > y = member' x (Just y) b
  | otherwise = True
