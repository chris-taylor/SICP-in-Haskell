f a b | a == 4 = 6
      | b == 4 = 6 + 7 + a
      | True   = 25

g a b | a > b = a
      | a < b = b
      | True  = (-1)

main = do
    putStrLn $ show 10
    putStrLn $ show (5 + 4 + 3)
    putStrLn $ show (9 - 1)
    putStrLn $ show (6 / 2)
    putStrLn $ show (2 * 4 + 4 - 6)
    let a = 3
    let b = a + 1
    putStrLn $ show (a + b + a * b)
    putStrLn $ show (a == b)
    putStrLn $ show (
        if (b > a) && b < a * b
            then b
            else a)
    putStrLn $ show (f a b)
    putStrLn $ show (2 + if b > a then b else a)
    putStrLn $ show (g a b * (a + 1))