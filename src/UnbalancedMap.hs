{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, InstanceSigs #-}

module UnbalancedMap (UnbalancedMap) where

import FiniteMap

data UnbalancedMap k a = E | T (UnbalancedMap k a) (k, a) (UnbalancedMap k a)
  deriving Show

instance Ord k => FiniteMap UnbalancedMap k where
  empty = E

  bind :: k -> v -> UnbalancedMap k v -> UnbalancedMap k v
  bind k v E = T E v E
  bind x v m@(T a y b)
    | x < y = T (insert x a) y b

  lookup = undefined
