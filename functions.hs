import Data.List
palindrome [] = []
palindrome(x:xs) = x:palindrome(xs) ++ [x]

isPalindrome [] = True
isPalindrome [_] = True
isPalindrome(x:xs) = (firstLast && middle)
  where firstLast = x == last(xs)
        middle = isPalindrome(init(xs))

byLength :: [a] -> [a1] -> Ordering
byLength a b = compare (length a) (length b)
sortByLength x = sortBy byLength x
sortByLengthExample = sortByLength ["third","1st","2nd!"]

intersperse' :: t -> [[t]] -> [t]
intersperse' _ [] = []
intersperse' seperator [a] = a
intersperse' seperator (x:xs) = x ++ [seperator] ++ (intersperse' seperator (xs))

intersperse'Example = intersperse' ' ' ["check","it","out"]
