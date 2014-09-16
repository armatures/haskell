data Tree' a = Node' a (Maybe (Tree' a)) ( Maybe (Tree' a))
                   deriving (Show)

data Tree a = Node a (Tree a) (Tree a) 
               | Empty
               deriving(Show)

height :: Tree a -> Int
height (Empty) = 0
height (Node _ l r)
    | left < right = 1 + right
    | otherwise = 1 + left
      where left = height l
            right = height r
