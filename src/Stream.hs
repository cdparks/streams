module Stream
  ( map
  , filter
  , enumFromTo
  , foldl'
  , sum
  ) where

import Prelude hiding (enumFromTo, filter, map, sum)

-- Think:
--   Stream  :: forall s a. (s -> Step   a  s ) -> s -> Stream a
--   unfoldr :: forall s a. (s -> Maybe (a, s)) -> s -> [a]
data Stream a = forall s . Stream
  { step :: s -> Step a s
  , seed :: s
  }

-- Non-recursive "list" structure with explicit skipping for, e.g. filter
data Step a s
  = Done
  | Yield a s
  | Skip s

-- | Convert a list to a 'Stream'
--
-- Delay inlining until phase 1 (phases run from high-to-low) so that
-- the rewrite rule has time to fire.
--
stream :: [a] -> Stream a
stream seed = Stream { seed, step }
 where
  step = \case
    [] -> Done
    x : xs -> Yield x xs
{-# INLINE [1] stream #-}

-- | Convert a 'Stream' to a list
--
-- Delay inlining until phase 1 (phases run from high-to-low) so that
-- the rewrite rule has time to fire.
--
unstream :: Stream a -> [a]
unstream Stream {..} = go seed
 where
  go s0 = case step s0 of
    Done -> []
    Skip s1 -> go s1
    Yield a s1 -> a : go s1
{-# INLINE [1] unstream #-}

-- Remove intermediate stream conversions
--
-- Run this any time before phase 1, which is when stream and unstream
-- are inlined.
--
{-# RULES "stream/unstream" [~1] forall s. stream (unstream s) = s #-}

map :: (a -> b) -> [a] -> [b]
map f = unstream . mapStream f . stream
{-# INLINE map #-}

filter :: (a -> Bool) -> [a] -> [a]
filter p = unstream . filterStream p . stream
{-# INLINE filter #-}

enumFromTo :: (Ord a, Enum a) => a -> a -> [a]
enumFromTo lo = unstream . enumFromToStream lo
{-# INLINE enumFromTo #-}

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z = foldl'Stream f z . stream
{-# INLINE foldl' #-}

sum :: Num a => [a] -> a
sum = foldl' (+) 0
{-# INLINE sum #-}

mapStream :: (a -> b) -> Stream a -> Stream b
mapStream f Stream {..} = Stream { step = next, seed }
 where
  next s0 = case step s0 of
    Done -> Done
    Skip s1 -> Skip s1
    Yield x s1 -> Yield (f x) s1
{-# INLINE mapStream #-}

filterStream :: (a -> Bool) -> Stream a -> Stream a
filterStream p Stream {..} = Stream { step = next, seed }
 where
  next s0 = case step s0 of
    Done -> Done
    Skip s1 -> Skip s1
    Yield x s1
      | p x -> Yield x s1
      | otherwise -> Skip s1
{-# INLINE filterStream #-}

enumFromToStream :: (Ord a, Enum a) => a -> a -> Stream a
enumFromToStream lo hi = Stream { step, seed = lo }
 where
  step n
    | n <= hi = Yield n (succ n)
    | otherwise = Done
{-# INLINE enumFromToStream #-}

foldl'Stream :: (b -> a -> b) -> b -> Stream a -> b
foldl'Stream f z Stream {..} = go z seed
 where
  go !acc s0 = case step s0 of
    Done -> acc
    Skip s1 -> go acc s1
    Yield x s1 -> go (f acc x) s1
{-# INLINE foldl'Stream #-}
