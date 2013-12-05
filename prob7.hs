sieve :: (Integral a) =>  [a] -> a -> Bool
sieve [] _ = False
sieve (x:xs) divisor = if mod divisor x == 0 then True else sieve xs divisor

sieve' :: (Integral a) => [a] -> a -> Bool
sieve' primes divisor = 
    let filt acc x = if mod divisor x == 0 then True else acc in
    foldl filt False primes

sieve'' :: (Integral a) => [a] -> a -> Bool
sieve'' primes divisor = 
    let filt x acc = if mod divisor x == 0 then True else acc in
    foldr filt False primes

--returns list of primes up to max
--primes :: (Integral a) => a -> [a]
primes max = primeRun max 3 [2] where
    primeRun maxPrime incr primes
        | length primes >= maxPrime = tail primes
        | otherwise = primeRun maxPrime (incr+2) newPrimes
        where newPrimes = if not $sieve' primes incr then incr:primes else primes 

prob7 = head $ primes 10002 
