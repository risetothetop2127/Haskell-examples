lastButOne :: [a] -> a
lastButOne []     = undefined
lastButOne [x]    = undefined
lastButOne [x, y] = x
lastButOne (x:xs) = lastButOne xs


lastButOne' :: [a] -> a
lastButOne' = head . tail . reverse

main = do
          print $ lastButOne  "abcd"
          print $ lastButOne' "abcd"
