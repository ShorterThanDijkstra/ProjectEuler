import Debug.Trace (trace)

multiples :: Int -> Int
multiples n =
  foldl
    (\acc i -> if i `mod` 3 == 0 || i `mod` 5 == 0 then trace (show i) acc + i else acc)
    0
    [1 .. n - 1]