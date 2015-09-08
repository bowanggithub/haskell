main = interact respondPalindromes

respondPalindromes = unlines . map (\xs -> if isPalindrome xs then "yes" else "no") . lines
    where isPalindrome xs = xs == reverse xs
