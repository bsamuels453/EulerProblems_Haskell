prob4 = maximum palindromes where
    palindromes = filter (isPalindrome) seed where
        seed = [x*y| x <- [100..999], y <- [100..999]]
        isPalindrome x = (show x) == (reverse $ show x)
