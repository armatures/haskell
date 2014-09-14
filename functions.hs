intersperse :: t -> [[t]] -> [t]
intersperse _ [] = []
intersperse seperator [a] = a
intersperse seperator (x:xs) = x ++ [seperator] ++ (intersperse seperator (xs))

intersperseExample = intersperse ' ' ["check","it","out"]

