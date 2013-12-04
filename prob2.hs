prob2 = sum $ takeWhile (<4000000) $ filter even fibs

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
