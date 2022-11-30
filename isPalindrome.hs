isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

main = do
          print $ isPalindrome  "abcd"
          print $ isPalindrome  "abba"
          print $ isPalindrome  "aba"
          print $ isPalindrome  ""