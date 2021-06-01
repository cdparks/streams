## What's happening to sumSquareEven?

1. Start

    ```haskell
    sumSquareEven :: Int -> Int
    sumSquareEven =
      Stream.sum
      . Stream.map square
      . Stream.filter even
      . Stream.enumFromTo 1
    ```

2. Inline definitions

    ```haskell
    sumSquareEven :: Int -> Int
    sumSquareEven =
      (foldl'Stream (+) 0 . stream)
      . (unstream . mapStream square . stream)
      . (unstream . filterStream even . stream)
      . (unstream . enumFromToStream 1)
    ```

3. (.) is associative, we can lose the parens...

    ```haskell
    sumSquareEven :: Int -> Int
    sumSquareEven =
      foldl'Stream (+) 0 . stream
      . unstream . mapStream square . stream
      . unstream . filterStream even . stream
      . unstream . enumFromToStream 1
    ```

4. ...and move things around to make occurrences of `stream . unstream`
   more obvious:

    ```haskell
    sumSquareEven :: Int -> Int
    sumSquareEven =
      foldl'Stream (+) 0
      . stream . unstream -- <------------
      . mapStream square
      . stream . unstream -- <------------
      . filterStream even
      . stream . unstream -- <------------
      . enumFromToStream 1
    ```

5. Apply rewrite rule `∀ s. stream (unstream s) = s` to eliminate
  `stream . unstream`:

    ```haskell
    sumSquareEven :: Int -> Int
    sumSquareEven =
      foldl'Stream (+) 0
      . mapStream square
      . filterStream even
      . enumFromToStream 1
    ```

5. GHC's regular inlining and optimization turns this into a single
   loop that operates over unboxed machine ints and only allocates
   a single constructor at the end:


    ```haskell
    sumSquareEven :: Int -> Int
    sumSquareEven (I# n) = go 0# 1#
     where
      go :: Int# -> Int# -> Int
      go acc x = case x <=# n of
        0# -> I# acc
        1# -> case remInt# acc 2# of
          0# -> go acc (x +# 1)
          1# -> go (acc +# (x *# x)) (x +# 1)
    ```
