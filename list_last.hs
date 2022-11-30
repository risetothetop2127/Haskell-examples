last' :: [a] -> a
last' []     = undefined
last' [x]    = x
last' (x:xs) = last' xs

last'' :: [a] -> a
last'' = head . reverse

main = do
          print $ last'  "abcd"
          print $ last'' "abcd"
