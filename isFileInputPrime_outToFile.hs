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
printLine handle line = do
                    case handleLine line of
                        (Just x) -> hPutStrLn handle line >> hPrint handle x
                        Nothing  -> return ()

main :: IO ()
main = do
          (inFile:outFile:_) <- getArgs
          inHandle  <- openFile inFile  ReadMode
          outHandle <- openFile outFile WriteMode
          hSetBuffering inHandle LineBuffering
          contents <- hGetContents inHandle
          mapM_ (\x -> printLine outHandle x) (lines contents)
          hClose inHandle
          hClose outHandle
          return ()

