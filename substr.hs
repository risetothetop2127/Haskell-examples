substr :: String -> Int -> Int -> String
substr "" _ _ = ""
substr xs s e
    | e < s         = undefined
    | s > length xs = ""
    | otherwise     = take (e - s) . drop s $ xs 

main = do
          print $ substr "abcdef" 0 4 -- "abcd"
          print $ substr "abcdef" 1 5 -- "bcde"
