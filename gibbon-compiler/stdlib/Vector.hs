{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Gibbon.Vector where

-- A standard library for Gibbon

--------------------------------------------------------------------------------
-- Vectors
--------------------------------------------------------------------------------

{-
#if MIN_VERSION_GLASGOW_HASKELL(8,6,5,0)

-- Built-ins. Uncomment this block in order to load this file in GHCi.

data Vector a = Vector

valloc :: Int -> Vector a
valloc = _builtin

length :: Vector a -> Int
length = _builtin

vnth :: Vector a -> Int -> a
vnth = _builtin

vslice :: Int -- Starting index
        -> Int -- length
        -> Vector a -> Vector a
vslice = _builtin

inplacevupdate :: Vector a -> Int -> a -> Vector a
inplacevupdate = _builtin

#endif

-}

--------------------------------------------------------------------------------

maxInt :: Int -> Int -> Int
maxInt a b = if a > b then a else b

minInt :: Int -> Int -> Int
minInt a b = if a < b then a else b

--------------------------------------------------------------------------------
-- Wrappers over Gibbon primitives.

-- Work: O(1)
-- Span: O(1)
length :: Vector a -> Int
{-# INLINE length #-}
length vec = vlength vec

-- Work: O(1)
-- Span: O(1)
nth :: Vector a -> Int -> a
{-# INLINE nth #-}
nth vec i = vnth vec i

-- Work: O(1)
-- Span: O(1)
slice :: Int -- Starting index
      -> Int -- length
      -> Vector a
      -> Vector a
{-# INLINE slice #-}
slice i n vec = vslice i n vec

-- sort :: Vector a -> (a -> a -> Int) -> Vector a
-- sort vec cmp = vsort vec cmp1

-- inplaceSort :: Vector a -> (a -> a -> Int) -> Vector a
-- inplaceSort vec cmp = inplacevsort vec cmp1

--------------------------------------------------------------------------------

-- Work: O(1)
-- Span: O(1)
isEmpty :: Vector a -> Bool
{-# INLINE isEmpty #-}
isEmpty vec = length vec == 0

-- Work: O(1)
-- Span: O(1)
singleton :: a -> Vector a
{-# INLINE singleton #-}
singleton x =
    let vec :: Vector a
        vec = valloc 1
        vec2 = inplacevupdate vec 0 x
    in  vec2

-- Work: O(1)
-- Span: O(1)
splitAt :: Int -> Vector a -> (Vector a, Vector a)
{-# INLINE splitAt #-}
splitAt n vec =
    -- Copied from Haskell's vector library.
    let len = length vec
        n'  = maxInt n 0
        m   = minInt n' len
        m'  = maxInt 0 (len - n')
    in (vslice 0 m vec, vslice m m' vec)

-- Work: O(1)
-- Span: O(1)
head :: Vector a -> a
{-# INLINE head #-}
head vec = nth vec 0

-- Work: O(1)
-- Span: O(1)
tail :: Vector a -> Vector a
{-# INLINE tail #-}
tail vec = slice 1 ((length vec)-1) vec

generate_loop :: Vector a -> Int -> Int -> (Int -> a) -> Vector a
generate_loop vec start end f =
    if start == end
    then vec
    else
      let vec1 = inplacevupdate vec start (f start)
      in generate_loop vec1 (start+1) end f

-- Work: O(n)
-- Span: O(n)
generate :: Int -> (Int -> a) -> Vector a
{-# INLINE generate #-}
generate n f =
    let n'  = maxInt n 0
        vec :: Vector a
        vec = valloc n'
        vec1  = generate_loop vec 0 n' f
    in vec1

generate_par_loop :: Int -> Vector a -> Int -> Int -> (Int -> a) -> Vector a
generate_par_loop cutoff vec start end f =
    -- if (end - start) <= cutoff
    if (end - start) <= 2
    then generate_loop vec start end f
    else
      let mid  = div (start + end) 2
          vec1 = spawn (generate_par_loop cutoff vec start mid f)
          vec2 = generate_par_loop cutoff vec mid end f
          _    = sync
      in vec2

-- Same default as Cilk.
defaultGrainSize :: Int -> Int
{-# INLINE defaultGrainSize #-}
defaultGrainSize n =
    let p = getNumProcessors
        grain = maxInt 1 (n / (8 * p))
    in minInt 2048 grain

-- Work: O(n)
-- Span: O(1)
generate_par :: Int -> (Int -> a) -> Vector a
{-# INLINE generate_par #-}
generate_par n f =
    let n'  = maxInt n 0
        vec :: Vector a
        vec  = valloc n'
        cutoff = defaultGrainSize n'
        vec1 = generate_par_loop cutoff vec 0 n' f
    in vec1

-- Work: O(n)
-- Span: O(n)
copy :: Vector a -> Vector a
{-# INLINE copy #-}
copy vec = generate (length vec) (\i -> nth vec i)

-- Work: O(n)
-- Span: O(1)
copy_par :: Vector a -> Vector a
{-# INLINE copy_par #-}
copy_par vec = generate_par (length vec) (\i -> nth vec i)

select :: Vector a -> Vector a -> Int -> a
{-# INLINE select #-}
select v1 v2 i =
    let len = length v1 in
    if i < len
    then vnth v1 i
    else vnth v2 (i - len)

-- Work: O(n)
-- Span: O(n)
append :: Vector a -> Vector a -> Vector a
{-# INLINE append #-}
append v1 v2 = generate
                    (length v1 + length v2)
                    (\i -> select v1 v2 i)

-- Work: O(n)
-- Span: O(1)
append_par :: Vector a -> Vector a -> Vector a
{-# INLINE append_par #-}
append_par v1 v2 = generate_par
                        (length v1 + length v2)
                        (\i -> select v1 v2 i)

-- Work: O(n)
-- Span: O(n)
map :: (a -> b) -> Vector a -> Vector b
{-# INLINE map #-}
map f vec = generate (length vec) (\i -> f (vnth vec i))

-- Work: O(n)
-- Span: O(1)
map_par :: (a -> b) -> Vector a -> Vector b
{-# INLINE map_par #-}
map_par f vec = generate_par (length vec) (\i -> f (vnth vec i))

-- Work: O(n)
-- Span: O(n)
update :: Vector a -> Int -> a -> Vector a
{-# INLINE update #-}
update vec i x = generate
                      (length vec)
                      (\j -> if i == j then x else vnth vec j)

-- Work: O(n)
-- Span: O(1)
update_par :: Vector a -> Int -> a -> Vector a
{-# INLINE update_par #-}
update_par vec i x = generate_par
                          (length vec)
                          (\j -> if i == j then x else vnth vec j)

-- Work: O(n)
-- Span: O(n)
foldl :: (b -> a -> b) -> b -> Vector a -> b
foldl f acc vec =
    let len = length vec in
    if len == 0
      then acc
      else if len == 1
        then f acc (vnth vec 0)
        else foldl f (f acc (vnth vec 0)) (vslice 1 (len-1) vec)

-- Work: O(n)
-- Span: O(n)
ifoldl :: (b -> Int -> a -> b) -> b -> Vector a -> b
{-# INLINE ifoldl #-}
ifoldl f acc vec = ifoldl1 0 f acc vec

-- Work: O(n)
-- Span: O(n)
ifoldl1 :: Int -> (b -> Int -> a -> b) -> b -> Vector a -> b
ifoldl1 idx f acc vec =
    let len = length vec in
    if len == 0
      then acc
      else if len == 1
        then f acc idx (vnth vec 0)
        else ifoldl1 (idx+1) f (f acc idx (vnth vec 0)) (vslice 1 (len-1) vec)

-- Work: O(n)
-- Span: O(log n)
foldl1_par :: (a -> a -> a) -> a -> Vector a -> a
foldl1_par f acc vec =
    let len = length vec in
    if len == 0
      then acc
      else if len == 1
        then f (vnth vec 0) acc
        else
          let mid  = div len 2
              tup  = splitAt mid vec
              v1   = fst tup
              v2   = snd tup
              acc1 = spawn (foldl1_par f acc v1)
              acc2 = (foldl1_par f acc v2)
              _    = sync
          in f acc1 acc2

-- Work: O(n)
-- Span: O(log n)
foldl2_par :: (b -> a -> b) -> b -> (b -> b -> b) -> Vector a -> b
foldl2_par f acc g vec =
    let len = length vec in
    if len == 0
      then acc
      else if len == 1
        then f acc (vnth vec 0)
        else
          let mid  = div len 2
              tup  = splitAt mid vec
              v1   = fst tup
              v2   = snd tup
              acc1 = spawn (foldl2_par f acc g v1)
              acc2 = (foldl2_par f acc g v2)
              _    = sync
          in g acc1 acc2

-- | It returns an Int because Gibbon doesn't have an IO monad yet.
printVec :: (a -> Int) -> Vector a -> Int
printVec f vec =
    let _ = printsym (quote "[")
        n = printVec_loop 0 (length vec) vec f
        _ = printsym (quote "]")
    in n

printVec_loop :: Int -> Int -> Vector a -> (a -> Int) -> Int
printVec_loop start end vec f =
    if start == end
    then 0
    else
        let x = head vec
            _ = f x
            _ = printsym (quote ",")
            rst = tail vec
        in printVec_loop (start+1) end rst f

-- Work: O(n)
-- Span: O(n)
snoc :: Vector a -> a -> Vector a
{-# INLINE snoc #-}
snoc vec x =
    let len = length vec
        vec2 :: Vector a
        vec2 = valloc (len + 1)
        vec3 = generate_loop vec2 0 len (\i -> nth vec i)
        vec4 = inplacevupdate vec3 len x
    in vec4

-- Work: O(n)
-- Span: O(n)
cons :: a -> Vector a -> Vector a
{-# INLINE cons #-}
cons x vec =
    let len = length vec
        vec2 :: Vector a
        vec2 = valloc (len + 1)
        vec3 = generate_loop vec2 1 (len+1) (\i -> nth vec (i-1))
        vec4 = inplacevupdate vec3 0 x
    in vec4

----------------------------------
-- TODO: review things after this.

filter_loop :: Vector Int -> Int -> Int -> Int -> Vector a -> Vector a -> Vector a
filter_loop idxs write_at start end from to =
    if start == end
    then to
    else
        let idx = nth idxs start
        in if idx == (-1)
           then filter_loop idxs write_at (start+1) end from to
           else
               let elt = nth from idx
                   to1 = inplacevupdate to write_at elt
               in filter_loop idxs (write_at+1) (start+1) end from to1

filter :: (a -> Bool) -> Vector a -> Vector a
{-# INLINE filter #-}
filter f vec =
    let idxs :: Vector Int
        idxs = generate (length vec) (\i -> if f (nth vec i) then i else (-1))
        num_ones = foldl (\(acc :: Int) (x :: Int) -> if x == (-1) then acc else acc + 1) 0 idxs
        to :: Vector a
        to = valloc num_ones
        len_idxs = length idxs
    in filter_loop idxs 0 0 len_idxs vec to
