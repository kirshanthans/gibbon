module ParAdd1ParTree where

data Tree = Leaf Int
          | Node Int Tree Tree

mkTree_seq :: Int -> Tree
mkTree_seq i =
  if i <= 0
  then Leaf 6765
  else
      let x = mkTree_seq (i-1)
          y = mkTree_seq (i-1)
      in Node i x y

mkTree :: Int -> Int -> Tree
mkTree cutoff i =
  if i <= 0
  then Leaf 6765
  else
      if i < cutoff
      then mkTree_seq i
      else let x = spawn (mkTree cutoff (i-1))
               y = mkTree cutoff (i-1)
               _ = sync
           in Node i x y

add1Tree_seq :: Tree -> Tree
add1Tree_seq tr =
  case tr of
    Leaf i     -> Leaf (i+1)
    Node i l r ->
      let l1 = (add1Tree_seq l)
          r1 = (add1Tree_seq r)
      in Node (i+1) l1 r1

add1Tree :: Int -> Tree -> Tree
add1Tree cutoff tr =
  case tr of
    Leaf i     -> Leaf (i+1)
    Node i l r ->
      if i < cutoff
      then add1Tree_seq tr
      else
        let l1 = spawn (add1Tree cutoff l)
            r1 = (add1Tree cutoff r)
            _  = sync
        in Node (i+1) l1 r1

sumTree_seq :: Tree -> Int
sumTree_seq foo =
  case foo of
    Leaf i     -> i
    Node i a b ->
      let x = sumTree_seq a
          y = sumTree_seq b
      in i + x + y

gibbon_main =
  let n = sizeParam
      cutoff = 19
      x = mkTree cutoff n
      y = iterate (add1Tree cutoff x)
  in (sumTree_seq y)