sumSquareRange n = sum $ map (^2) $ take n [1..]

squareSumRange n = (sum $ take n [1..])^2

prob6 = (squareSumRange 10) - (sumSquareRange 10)
