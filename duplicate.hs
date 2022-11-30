duplicate :: [a] -> [a]
duplicate []     = []
duplicate (x:xs) = [x, x] ++ duplicate xs

main = print $ duplicate "abc"