data Direction = Lturn | Straight | Rturn
                 deriving (Show)

turn :: (Num a, Ord a) => (a, a) -> (a, a) -> (a, a) -> Direction
turn (x1,y1) (x2,y2) (x3,y3) | cross > 0  = Lturn
                             | cross == 0 = Straight
                             | cross < 0  = Rturn
                               where cross = (x2 - x1)*(y3 - y1) - (y2 - y1)*(x3 - x1)

turns (x1:x2:x3:xs) = (turn x1 x2 x3):turns(x2:x3:xs)
turns _ = []
