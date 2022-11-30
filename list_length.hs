len :: [a] -> Int
len [x]    = 1
len (x:xs) = 1 + (len xs)

--len' :: [a] -> Int
--len' [x]   = [x

main = do
          print $ len  "abcd"
--          print $ len' "abcd"
