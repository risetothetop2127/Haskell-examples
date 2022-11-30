isPrime :: Int -> Bool
isPrime n
    | n <= 1    = False
    | otherwise = not $ any (\x -> n `mod` x == 0) [2,3..(n-1)]

handleLine :: String -> Maybe Bool
handleLine numberString =
        if length numberString == 0
            then Nothing
            else let number = read numberString :: Int
                 in Just (isPrime number)

main = do
         numberString <- getLine
         case handleLine numberString of
           (Just x) -> print x >> main
           Nothing  -> return()

