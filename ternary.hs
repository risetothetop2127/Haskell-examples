infixr 1 ?
True  ? x = const x
False ? _ = id


x = True  ? "ohai" $ "obai"
y = False ? "ohai" $ "obai"
main = do putStrLn x
          putStrLn y
