module CurveTest where
 
import Curve
 
-- Create a curve to work with
c1 :: Curve
c1 = Curve (map point [(-2,2),(-1,1),(0,0)])
c2 :: Curve
c2 = Curve (map point [(0,1),(1,2),(2,3)])
 
-- Test for 'point' function
testPoint :: Bool
testPoint = point (1,2) == (Point 1 2)
 
-- Test for 'curve' function
testCurve :: Bool
testCurve = check1 == actual1 && check2 == actual2
	where check1 = curve (point (1,2)) [point (2,2)]
	      actual1 = Curve [point (1,2), point (2,2)]
	      check2 = curve (point (1,2)) []
	      actual2 = Curve [point(1,2)]
 
-- Testing for equality
testEq :: Bool
testEq = point (0,0) == point(0.009, 0.009) && point(0,0) /= point(0.01,0.01)
 
-- Test for 'connect' function
testConnect :: Bool
testConnect = check1 == actual1
	where check1 = connect c1 c2
	      actual1 = Curve ( map point [(-2,2),(-1,1),(0,0),(0,1),(1,2),(2,3)])
 
-- Test for 'rotate' function
testRotate :: Bool
testRotate = check1 == actual1 && check2 == actual2
	where check1 = rotate c1 32
	      actual1 = Curve (map point [(-0.636, 2.76), (-0.318, 1.38), (0,0)])
	      check2 = rotate c1 (-30)
	      actual2 = Curve (map point [(-2.732,0.732), (-1.366, 0.366), (0,0)])
 
-- Test for 'translate' function
testTranslate :: Bool
testTranslate = check1 == actual1 && check2 == actual2
	where check1 = translate c1 (point (1,1))
	      actual1 = Curve (map point [(1,1), (2,0), (3,-1)])
	      check2 = translate c1 (point (-3,2))
	      actual2 = Curve (map point [(-3,2),(-2,1),(-1,0)])
 
-- Test for 'reflect' function
testReflect :: Bool
testReflect = check1 == actual1 && check2 == actual2
	where check1 = reflect c1 Horizontal 2
	      actual1 = Curve (map point [(-2,2), (-1,3), (0,4)])
	      check2 = reflect c1 Vertical 2
	      actual2 = Curve (map point [(6,2), (5,1), (4,0)])
 
-- Test for 'toList' function
testToList :: Bool
testToList = check == actual
	where check = toList c1
	      actual = map point [(-2,2), (-1,1), (0,0)]
 
-- Test for 'bbox' function
testBBox :: Bool
testBBox = check == actual
	where check = bbox c1
	      actual = (point (-2,0), point (0,2))
 
-- Test for 'width' function
testWidth :: Bool
testWidth = check == actual
	where check = width c1
	      actual = 2.0
 
-- Test for 'height' function
testHeight :: Bool
testHeight = check == actual
	where check = height c1
	      actual = 2.0
 
runTests :: IO ()
runTests = do
print testPoint
print testCurve
print testEq
print testConnect
print testRotate
print testTranslate
print testReflect
print testToList
print testBBox
print testWidth
print testHeight
