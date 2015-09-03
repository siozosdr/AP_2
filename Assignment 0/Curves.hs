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

curve :: Point -> [Point] -> Curve

connect :: Curve -> Curve -> Curve

rotate :: Curve -> Double -> Curve

translate :: Curve -> Point -> Curve

data Line = Vertical Double | Horizontal Double

reflect :: Curve -> Line -> Curve

bbox :: Curve -> (Point, Point)

toList :: Curve -> [Point]