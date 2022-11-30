compress :: (Eq a) => [a] -> [a]
compress []  = []
compress [x] = [x] -- also can do (x:[]) to be clear what the pattern is.
compress (x:y:xs)
    | x == y    = x : (compress xs)
    | otherwise = x : y : (compress xs)

main = do
          print $ compress  "abcd"
          print $ compress  "abba"
          print $ compress  "abccd"
          print $ compress  "aaaabbbbccccdddd"
