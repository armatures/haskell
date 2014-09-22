import Test.HUnit
import Plane

turnsTest = TestCase $ assertEqual "turns on a list" [Rturn,Lturn,Straight] (turns [(0,0),(0,1),(1,1),(1,2),(1,3)]) 
