pyth n =[ (x, y, z)
        | x <- [1 .. n] 
        , y <- [x .. n] 
        , z <- [y .. n] 
        , x ^ 2 + y ^ 2 == z ^ 2 ]

main :: IO ()
main =
  putStr $ show $ pyth 1000
