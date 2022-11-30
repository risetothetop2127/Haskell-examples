dropNth :: [a] -> Int -> [a]
dropNth x n = dropNth' x n n

dropNth' :: [a] -> Int -> Int -> [a]
dropNth' []     _     _ = []
dropNth' _      0     _ = []
dropNth' (x:xs) limit 1 = dropNth' xs  limit limit
dropNth' (x:xs) limit n = [x] ++ dropNth' xs limit (n - 1)

-- dropNth' [1,2,3] 2 2
--   dropNth' [2,3] 2 1
--     dropNth' [3] 2 0
--       dropNth' [] 2 2
--       <- []
--     <- []
--   <- [2]
-- <- [1,2]
main = do
          print $ dropNth "abcdabcda" 1 -- ""
          print $ dropNth "abcdabcda" 2 -- "acaca"
          print $ dropNth [1,2,3,4,5,6,7,8] 2 -- [1,3,5,7]
          print $ dropNth [1,2,3,4,5,6,7,8] 1 -- []
          print $ dropNth [1,2,3] 2 -- [1,3]
          print $ dropNth [1,2,3,4] 2 -- [1,3]
          print $ dropNth ([]::[Char]) 1 -- []
          print $ dropNth ([]::[Char]) 100 -- []