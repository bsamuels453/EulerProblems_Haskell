prob10 = sum $ primes $ 2:[3, 5 .. 1999999]
primes  [] = []
primes (x:xs) 
    | x < 1500 = x : primes (filterSieve x xs)
    | otherwise = x:xs

filterSieve _ [] = []
filterSieve divisor (x:xs)
    | mod x divisor == 0 = filterSieve divisor xs
    | otherwise = x : filterSieve divisor xs
