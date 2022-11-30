isPrime :: Int -> Bool
isPrime n
    | n <= 1    = False
    | otherwise = not $ any (\x -> n `mod` x == 0) [2,3..(n-1)]

main = do
          print $ isPrime 10 -- False
          print $ isPrime 1  -- False
          print $ isPrime 2  -- True
          print $ isPrime 3  -- True
          print $ isPrime 13 -- True