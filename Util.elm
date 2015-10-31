module Util where

import Set
import List
import Transform2D exposing (matrix, Transform2D)
import Graphics.Collage exposing (groupTransform, Form)

type alias Dir = Float
type alias Point = (Float, Float)
type alias Edge = (Point, Point)
type alias Side = List Point
type alias Polygon = List Side
type alias Pattern = List (Point, Dir, Int)
type alias Stamp = { shape : Polygon,
                     pattern : Pattern, 
                     link : Link }
type alias Link = Int -> Point -> (Int, Point)

largeNumber = 999999999

odd : Int -> Bool
odd x = x%2==1

edgeToSide : Edge -> Side
edgeToSide (s,e) = [s,e]

sideToEdge : Side -> Edge
sideToEdge (x::xs) = (x, end xs)

rem2pi : Float -> Float
rem2pi x = if x >= 2*pi then rem2pi <| x-(2*pi) else x

exteriorAngle : Int -> Float
exteriorAngle n = let n' = toFloat n in ((n'-2) * pi) / n'

fuzzyEquals : Float -> Float -> Bool
fuzzyEquals a b = abs (a-b) < 0.01

trunc : Float -> Float
trunc x = toFloat (round (x * 1000)) / 1000

unions : List (Set.Set Point) -> Set.Set Point
unions = List.foldl Set.union Set.empty

distSquared : Point -> Point -> Float
distSquared (x1,y1) (x2,y2) = (x1-x2)^2 + (y1-y2)^2

dist : Point -> Point -> Float
dist p1 p2 = sqrt <| distSquared p1 p2

(./) : Point -> Float -> Point
(./) (x,y) k = (x/k, y/k)
infixr 7 ./

(.*) : Float -> Point -> Point
(.*) k (x,y) = (k*x, k*y)
infixr 7 .*

(.+) : Point -> Point -> Point
(.+) (x,y) (x',y') = (x+x', y+y')
infixr 6 .+

(.-) : Point -> Point -> Point
(.-) (x,y) (x',y') = (x-x', y-y')
infixr 6 .-

proj : (Float, Float) -> (Float, Float) -> Float
proj v r = dot v r / (mag v)

mag : (Float, Float) -> Float
mag (x,y) = sqrt (x^2+y^2)

dot : (Float, Float) -> (Float, Float) -> Float
dot (x1,y1) (x2,y2) = x1*x2 + y1*y2

end : List a -> a
end xs = case List.head (List.reverse xs) of Just v -> v

replaceList : Int -> a -> List a -> List a
replaceList i v l = (List.take i l) ++ [v] ++ (List.drop (i+1) l)

delete : Int -> List a -> List a
delete i xs = (List.take i xs) ++ (List.drop (i+1) xs)

get : Int -> List a -> a
get i (x::xs) = if i == 0 then x else get (i-1) xs

getIndexOf : a -> List a -> Int
getIndexOf a (x::xs) = if
    | a == x -> 0
    | otherwise -> 1 + getIndexOf a xs

mapBetween : (a -> a -> b) -> List a -> List b
mapBetween f (x::y::xs) = if
    | xs == [] -> [f x y]
    | otherwise -> f x y :: mapBetween f (y::xs)

unitize : (Float,Float) -> Point
unitize v = v ./ (mag v)

perp : (Float, Float) -> (Float, Float)
perp (x,y) = (y,-x)

toRelativeCoords :Point -> Edge -> Point
toRelativeCoords p (s,e) = 
    let 
        se = unitize (e .- s)
        se' = perp se
        x = p .- s
    in 
        (proj se x, proj se' x)

fromRelativeCoords : Point -> Edge -> Point
fromRelativeCoords (x,y) (s,e) =
    let
        se = unitize (e .- s)
        se' = perp se
    in
        s .+ (x .* se .+ y .* se')

negateY : Point -> Point
negateY (x,y) = (x,-y)

flipX : Form -> Form
flipX form = groupTransform (matrix -1 0 0 1 0 0) [form]

flipY : Form -> Form
flipY form = groupTransform (matrix 1 0 0 -1 0 0) [form]