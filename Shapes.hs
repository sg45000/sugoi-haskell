module Shapes (
    Point(..),
    Shape(Circle, Rectangle),
    area,
    nudge,
    baseCircle,
    baseRect
) where

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x1 - x2) * (abs $ y1 - y2)


nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) xa ya = Circle (Point (x + xa) (y + ya)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) xa ya
 = Rectangle (Point (x1 + xa) (y1 + ya)) (Point (x2 + xa) (y2 + ya))

baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)

baseRect :: Float -> Float -> Shape
baseRect h w = Rectangle (Point 0 0) (Point h w)
