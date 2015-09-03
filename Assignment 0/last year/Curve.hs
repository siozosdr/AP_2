-- Sokratis Siozos-Drosos
-- dnb823@ku.dk

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


data Point = Point Double Double 
point :: (Double, Double) -> Point
point (x, y) = Point x y

data Curve = Curve [Point] 

curve :: Point -> [Point] -> Curve
curve p lp = Curve (p:lp)

translate :: Curve -> Point -> Curve
translate (Curve []) _ = Curve []
translate (Curve (Point sx sy : pl)) (Point px py) = Curve $ map (translatePoint tv) (Point sx sy : pl)
  where
    -- translation vector
    tv = Point (px - sx) (py - sy)

-- helper function for translate, translates a single point
-- tx,ty is the translation vector found by subtracting the starting point
-- from the translation point
translatePoint :: Point -> Point -> Point
translatePoint (Point tx ty) (Point x y) = Point (x + tx) (y + ty)

rotate :: Curve -> Double -> Curve
rotate (Curve pl) deg = Curve (map(rotatePoint deg) pl)

-- helper function for rotate, rotates a single point
rotatePoint :: Double -> Point -> Point
rotatePoint deg (Point x y) = Point x' y'
  -- convertion of degrees to radians
  where 
    r  = deg * pi / 180
    x' = x * cos r + y * sin r
    y' = y * cos r - x * sin r 

connect :: Curve -> Curve -> Curve
connect (Curve pl1) (Curve pl2) = Curve (pl1 ++ pl2)


data Axis = Vertical | Horizontal
reflect :: Curve -> Axis -> Double -> Curve
reflect (Curve pl) axis d   = Curve (map(reflectPoint d axis) pl)

-- helper function for reflect to reflect a single point
reflectPoint ::  Double -> Axis -> Point -> Point
reflectPoint d Vertical  (Point x y)  = Point (x - 2*(x - d)) y
reflectPoint d Horizontal (Point x y) = Point x (y - 2*(y - d))

bbox :: Curve -> (Point, Point)
bbox (Curve []) = (Point 0 0, Point 0 0)
bbox (Curve (sp:pl)) = (minbbox pl sp , maxbbox pl sp)
  where			
  -- helper functions for bbox to find point with minimum coordinates and point with  maximumu coordinates
    minbbox :: [Point] -> Point -> Point
    minbbox pl' p = foldl (flip minPoint) p pl'
       where
         minPoint :: Point -> Point -> Point
	 minPoint (Point x1 y1) (Point x2 y2) = Point (minCoord x1 x2) (minCoord y1 y2)
	    where
	      minCoord :: Double -> Double -> Double
	      minCoord x y = if x < y then x else y
    maxbbox :: [Point] -> Point -> Point
    maxbbox pl'' p = foldl (flip maxPoint) p pl'' 
	where 
	  -- helper functions to compare two points and find the one that is greater
	  maxPoint :: Point -> Point -> Point
	  maxPoint (Point x1 y1) (Point x2 y2) = Point (maxCoord x1 x2) (maxCoord y1 y2)
	    where
	      maxCoord :: Double -> Double -> Double
	      maxCoord x y = if x > y then x else y
	 
				

height :: Curve -> Double
height c = abs (y1 - y2)
	 where (Point _ y1, Point _ y2) = bbox c
	

width :: Curve -> Double
width c = abs (x1 - x2)
	where (Point x1 _, Point x2 _) = bbox c

toList :: Curve -> [Point]
toList (Curve pl) = pl

toSVG :: Curve -> String
toSVG c = let
	w = (ceiling . width  $ c) :: Integer
	h = (ceiling . height  $ c) :: Integer
	header = printf "<svg xmlns = \"http://www.w3.org/2000/svg\" \n\t width=\"%dpx\" height=\"%dpx\" version=\"1.1\"> \n<g>\n" w h
	listOfPoints = toList c
	linesSVG = listToLines Nothing listOfPoints
	footer = printf "</g> \n</svg>"
	in header ++ linesSVG ++ footer
	  where
	    -- helper function to produce the lines
	    -- Use of Maybe Point type to ensure that the first argument of
	    -- the function will always have either Just one Point or 
	    -- no Points, so that we will always be able to have pairs of
	    -- points to print.
	    listToLines :: Maybe Point -> [Point] -> String
	    listToLines _ [] = ""
	    listToLines Nothing (ph:pl) = listToLines (Just ph) pl
	    listToLines (Just (Point x y)) (Point hx hy: pl) = printf "<line style=\"stroke-width: 2px; stroke: black; fill:white\" \n\t x1=\"%.2f\" x2=\"%.2f\" y1=\"%.2f\" y2=\"%.2f\" /> \n" x hx y hy ++ listToLines (Just (Point hx hy)) pl

toFile :: Curve -> FilePath -> IO ()
toFile c f = writeFile f (toSVG c) 

-- hilbert implementation
hilbert :: Curve -> Curve
hilbert c = c0 `connect` c1 `connect` c2 `connect` c3
   where  w = width c
          h = height c
          p = 6

          ch = reflect c Horizontal 0

          c0 = ch `rotate` (-90) `translate` point (w+p+w, h+p+h)
          c1 = c `translate` point (w+p+w, h)
          c2 = c
          c3 = ch `rotate` 90 `translate` point (0, h+p)

-- test cases
foo,foo2,foo3 :: IO()
foo =putStrLn . toSVG $ curve (point (0,10)) (map point [(5,0),(10,10),(0,10)])
foo2 = toFile (curve (point (0,10)) (map point [(5,0),(10,10),(0,10)])) "tosvg/1.svg"
foo3 = toFile (hilbert $ hilbert $ hilbert $ hilbert $ curve (point (0,0)) []) "tosvg/hilbert.svg"  
