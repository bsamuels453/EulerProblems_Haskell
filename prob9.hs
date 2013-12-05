triplets = [(x,y,z)| x<-[1..800], y<-[1..800], z<-[1..800], x**2+y**2==z**2]

sum1000 ((a,b,c):xs)
    | a+b+c==1000 = (a,b,c)
    | otherwise = sum1000 xs
sum1000 [] = (0,0,0)

prob9 = a*b*c where
    (a,b,c) = sum1000 triplets
