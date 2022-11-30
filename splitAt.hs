splitAt' :: [a] -> Int -> ([a], [a])
splitAt' xs n
    | n < 0         = ([], xs)
    | n > length xs = (xs, [])
    | otherwise     = splitAt'' ([], xs) n 

splitAt'' :: ([a], [a]) -> Int -> ([a], [a])
splitAt'' (start, end) 0 = (start, end)
splitAt'' (xs, (y:ys)) n = splitAt'' (xs ++ [y], ys) (n - 1)

main = do
          print $ splitAt' "abcd" 2    -- ("ab", "cd")
          print $ splitAt' "abcd" 4    -- ("abcd", "")
          print $ splitAt' "abcd" 10   -- ("abcd", "")
          print $ splitAt' "abcd" 0    -- ("", "abcd")
          print $ splitAt' ""     1000 -- ("", "")
          print $ splitAt' ""     (-1) -- ("", "")