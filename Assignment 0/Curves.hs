module Curve where
import Text.Printf

instance Eq Point where
	Point x y == Point a b = (abs(x-a) < 0.01 ) && (abs(y-b) < 0.01)

-- customize Show to round up to 3 decimals when showing Points
-- mainly for debugging
instance Show Point where
	show (Point x y) = "Point " ++ show x' ++ " " ++ show y'
		where   x',y' :: Double
			x' = fromIntegral (round (x * 1000) :: Integer) / 1000
	      		y' = fromIntegral (round (y * 1000) :: Integer) / 1000	


point :: (Double, Double) -> Point

pointx :: Point -> Double

pointy :: Point -> Double

data Curve = Curve [Point]
curve :: Point -> [Point] -> Curve
curve p lp = Curve (p:lp)

connect :: Curve -> Curve -> Curve
connect (Curve c1) (Curve c2) = Curve (c1 ++ c2)

rotate :: Curve -> Double -> Curve
rotate (Curve []) _ = (Curve [])
rotate (Curve (Point x y : pl)) deg = Curve (map(rotatePoint deg) pl)

-- helper function for rotate
-- rotates a single point based on a double d
rotatePoint :: double -> Point -> Point
rotatePoint d (Point x y) = (Point x' y')
	where
		r  = deg * pi / 180
    	x' = x * cos r + y * sin r
    	y' = y * cos r - x * sin r 

translate :: Curve -> Point -> Curve
translate (Curve []) _ = (Curve [])
translate (Curve (Point x y: pl)) (Point px py) = Curve $ map (translatePoint tv) (Point x y: pl)
	where 
		--translation vector
		tv = Point (px - x) (py - y)

-- helper for translate
translatePoint :: Point -> Point -> Point
translatePoint (Point x y) (Point tx ty) = Point (x+tx) (y+ty)

data Line = Vertical Double | Horizontal Double

reflect :: Curve -> Line -> Curve

bbox :: Curve -> (Point, Point)

toList :: Curve -> [Point]