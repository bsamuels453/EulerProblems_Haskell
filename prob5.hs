--i--prob5 :: Integer
--prob5 = foldr (lcm) 1 [1..20]

prob5' :: Integer
prob5' = solve 20  [11,13,14,16,17,18,19,20] where
    solve incr range = 
        if not $ indivisible incr range then incr 
        else solve (incr+20) range where
        indivisible v range = elem False $ map divisible range where
            divisible x = mod v x == 0


--2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
--What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
prob5 :: Integer
prob5 = solve 2 [11,13,14,16,17,18,19,20] where
    solve incr range
        | not (indivisible incr range) =incr 
        | otherwise                    = solve (incr+20) range
      where
        indivisible v range = elem False $ map divisible range
          where
            divisible x = mod v x == 0
