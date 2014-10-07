import Test.HUnit
import Plane

turnsTest = TestCase $ assertEqual "turns on a list" [Rturn,Lturn,Straight] (turns [(0,0),(0,1),(1,1),(1,2),(1,3)]) 
groupByYTest = TestCase $ assertEqual "groups" [[(0,2)], [ (2,1), (1, 1)]] (groupByY [(0,2), (2,1), (1, 1)])

firstTest = TestCase $ assertEqual "takes the point with the smallest y-coordinate" (1,-10) (firstPoint [(0,0), (1,-10), (-1, 1)])
firstTest1 = TestCase $ assertEqual "in a tie, takes the point with the smallest x-coordinate" (1,1) (firstPoint [(0,2), (2,1), (1, 1)])
runFirstTests = runTestTT $ TestList [firstTest, firstTest1]

sortByPolarAngleTest = TestCase $ assertEqual "orders the points by polar angle with the x-axis and the bottom-left point" [(1,-1),(1,1),(-1,1)] (sortByPolarAngle [(-1,-1),(-1,1),(1,1),(1,-1)])
otherPointsTest = TestCase $ assertEqual "all the points that aren't the first point" [(1,-1),(1,1),(-1,1)] (otherPoints [(1,-1),(-1,-1),(1,1),(-1,1)])
