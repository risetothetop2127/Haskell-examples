import System.Environment
import System.IO

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

--printLine :: String -> IO ()
printLine line = do
                    case handleLine line of
                        (Just x) -> putStrLn line >> print x
                        Nothing  -> return ()

main :: IO ()
main = do
          (filename:_) <- getArgs
          handle <- openFile filename ReadMode
          hSetBuffering handle LineBuffering
          contents <- hGetContents handle
          mapM_ printLine (lines contents)
          return ()

