module JoinList where

import Sized

-- The JoinList type was provided with the homework
data JoinList m a = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

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
indexJ :: (Sized m, Monoid m) => Int -> JoinList m a -> Maybe a
indexJ i jl | i < 0 || i >= getSize (size jl) = Nothing
indexJ 0 (Single _ a) = Just a
indexJ i (Append _ l r) = let leftSize = getSize (size l) in
   if i < leftSize then indexJ i l else indexJ (i-leftSize) r

