## Resources

* [Stream Fusion: From Lists to Streams to Nothing at All](https://www.researchgate.net/publication/221241130_Stream_Fusion_From_Lists_to_Streams_to_Nothing_at_All) by Duncan Coutts, Roman Leshchinskiy, and Don Stewart
* GHC Docs:
    * [INLINE and NOINLINE](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/pragmas.html#inline-and-noinline-pragmas)
    * [Rewrite Rules](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/rewrite_rules.html#rewrite-rules)
    * [Phase Control](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/pragmas.html#phase-control)

## What's happening to sumSquareEven?

Note:

```haskell
stream   :: [a] -> Stream a
unstream :: Stream a -> [a]
```

_and_ `∀ s. stream (unstream s) = s`

1. Start

    ```haskell
    sumSquareEven :: Int -> Int
    sumSquareEven = sum . map square . filter even . enumFromTo 1
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

3. `(.)` is associative. We can lose the parens...

    ```haskell
    sumSquareEven :: Int -> Int
    sumSquareEven =
      foldl'Stream (+) 0 . stream
      . unstream . mapStream square . stream
      . unstream . filterStream even . stream
      . unstream . enumFromToStream 1
    ```

4. ...and move things around to make occurrences of `stream . unstream` more obvious:

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

5. Apply rewrite rule `∀ s. stream (unstream s) = s` to eliminate `stream . unstream`:

    ```haskell
    sumSquareEven :: Int -> Int
    sumSquareEven =
      foldl'Stream (+) 0
      . mapStream square
      . filterStream even
      . enumFromToStream 1
    ```

5. GHC's regular inlining and optimization turns this into a single loop that operates over unboxed machine ints that only allocates a single `I#` constructor at the end.

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

6. This loop looks like a recursive function, but it's really just a [jump with arguments](https://www.pauldownen.com/publications/pldi17.pdf):

    ```haskell
    sumSquareEven :: Int -> Int
    sumSquareEven (I# n) = joinrec {
      $wgo :: Int# -> Int# -> Int
      $wgo acc x = case x <=# n of
        0# -> I# acc
        1# -> case remInt# acc 2# of
          0# -> jump $wgo acc (x +# 1)
          1# -> jump $wgo (acc +# (x *# x)) (x +# 1)
    } in jump $wgo 0# 1#
    ```

7. Finally, this gets inlined directly into `main`:

    ```haskell
    main :: IO ()
    main = do
      line <- getLine                       -- skipping inlined getline
      case readMaybe line of                -- skipping inlined readMaybe
        Just (I# n) -> joinrec {
          $wgo :: Int# -> Int# -> Int
          $wgo acc x = case x <=# n of
            0# -> I# acc
            1# -> case remInt# acc 2# of
              0# -> jump $wgo acc (x +# 1)
              1# -> jump $wgo (acc +# (x *# x)) (x +# 1)
          } in print (jump $wgo 0# 1#)      -- skipping inlined print
        Nothing -> main
    ```
