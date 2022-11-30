reduce _  start []   = start
reduce fn start rest = reduce fn (fn start (head rest)) (tail rest)

x = reduce (+) 0  [1,2,3,4,5,6,7]
y = reduce (-) 28 [1,2,3,4,5,6,7]

main = do putStrLn (show x)
          putStrLn (show y)

