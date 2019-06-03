{-# LANGUAGE FlexibleInstances #-}

module JoinList where

import Sized
import Scrabble
import Buffer

-- The JoinList type was provided with the homework
data JoinList m a = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- JoinList to list function provided with the homework
jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- Exercise 1
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l r = Append (tag l <> tag r) l r

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- Exercise 2

-- For convenience, I make any join list with a Sized, Monoid annotation Sized
instance (Sized m, Monoid m) => Sized (JoinList m a) where
  size = size . tag

-- Find the join list element at the specified index. It should be the case that
-- (indexJ i jl) == (jlToList jl !!? i) for any index `i` and join-list `jl`
indexJ :: (Sized m, Monoid m) =>
          Int -> JoinList m a -> Maybe a
indexJ i jl
  | i < 0 = Nothing
  | i >= getSize (size jl) = Nothing
indexJ 0 (Single _ a) = Just a
indexJ i (Append _ l r) = let leftSize = getSize (size l) in
   if i < leftSize then indexJ i l else indexJ (i-leftSize) r

dropJ :: (Sized m, Monoid m) =>
         Int -> JoinList m a -> JoinList m a
dropJ i jl
  | i <= 0 = jl
  | i >= getSize (size jl) = Empty
dropJ _ Empty = Empty
dropJ _ (Single _ _) = Empty
dropJ i (Append _ l r) = let leftSize = getSize (size l) in
  if i < leftSize then (dropJ i l) +++ r else dropJ (i-leftSize) r


takeJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
takeJ i jl
  | i <= 0 = Empty
  | i >= getSize (size jl) = jl
takeJ _ Empty = Empty
takeJ _ jl@(Single _ _) = jl
takeJ i (Append _ l r) = let leftSize = getSize (size l) in
  if i <= leftSize then takeJ i l else l +++ takeJ (i-leftSize) r

-- Exercise 3

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- Exercise 4

-- note: this code relies on the rather strange property that a pair with the
-- second item is sized, is considered sized by that

instance Buffer (JoinList (Score, Size) String) where
  toString     = unlines . jlToList
  fromString   = fromLines . lines
    -- tried to come up with the most efficient implementation I could. We want
    -- the resulting tree to be completely balanced (i.e. at every node, the
    -- size of the child trees differs by at most one). We also want to avoid
    -- recursing down the list repeatedly. This implementation does so once to
    -- get the length of the list and again as it builds the tree
    where fromLines xs = fst $ mkTree (length xs) xs
          -- Makes a completely balanced tree out of the first n elements of a
          -- list and returns a pair of the tree and any elements after the
          -- the first n.
          mkTree 0 xs     = (Empty, xs)
          mkTree 1 (x:xs) = (Single (scoreString x, Size 1) x, xs)
          mkTree n xs     = let half = n `div` 2
                                (l, ys) = mkTree half xs
                                (r, zs) = mkTree (n-half) ys in
                              (l +++ r, zs)
  line         = indexJ
  replaceLine n l b = takeJ n b +++ fromString l +++ dropJ (n+1) b
  numLines     = getSize . snd . tag
  value        = getScore . fst . tag
