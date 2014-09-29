import Test.HUnit
import Plane

turnsTest = TestCase $ assertEqual "turns on a list" [Rturn,Lturn,Straight] (turns [(0,0),(0,1),(1,1),(1,2),(1,3)]) 
groupByYTest = TestCase $ assertEqual "groups" [[(0,2)], [ (2,1), (1, 1)]] (groupByY [(0,2), (2,1), (1, 1)])
