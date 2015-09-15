module Stamps where

import Graphics.Collage exposing (toForm, group, move, rotate, Form, Shape, path, polygon, rect, filled, outlined, traced, solid, collage)
import Graphics.Element exposing (image, layers, show, above, Element)
import Color exposing (red, black, toRgb, rgb)
import Set
import List as List

type alias Dir = Float
type alias Point = (Float, Float)
type alias Edge = (Point, Point)
type alias Side = List Point
type alias Polygon = List Side
type alias Pattern = List (Point, Dir)
type alias Stamp = (Polygon, Pattern)
type alias Link = Int -> Point -> (Int, Point)
type alias Model = { stamp : Stamp,
                     link : Link,
                     editing : Bool,
                     lastPoint: (Int,Int),
                     debug : String }


width = 1000
height = 500

edgeToSide : Edge -> Side
edgeToSide (s,e) = [s,e]

rem2pi : Float -> Float
rem2pi x = if x >= 2*pi then rem2pi <| x-(2*pi) else x

exteriorAngle : Int -> Float
exteriorAngle n = let n' = toFloat n in ((n'-2) * pi) / n'

makePoint : Float -> Float -> Point -> Point
makePoint dist angle (x,y) = 
    let
        dir = rem2pi <| angle 
    in ( x + dist * (cos dir), y + dist * (sin dir) ) 

makeEdge : Float -> Float -> Edge -> Edge
makeEdge len angle (_, (x,y)) = ((x,y), makePoint len angle (x,y))

ngon : Int -> Float -> Polygon
ngon n size =
    let
        angle = pi - (exteriorAngle n)
    in
        List.map edgeToSide <| List.scanl (makeEdge size) ((0,0), (size,0)) <| List.map (\x->x*angle) [1.0 .. (toFloat n-1)]

hexagon : Polygon
hexagon = ngon 6 50

fuzzyEquals : Float -> Float -> Bool
fuzzyEquals a b = abs (a-b) < 0.01

trunc : Float -> Float
trunc x = toFloat (round (x * 1000)) / 1000

unions : List (Set.Set Point) -> Set.Set Point
unions = List.foldl Set.union Set.empty

addNeighbors : (Point -> Pattern) -> List (Point, Dir) -> Set.Set (Point, Dir) -> Set.Set (Point, Dir)
addNeighbors neighbors (((x,y),dir)::xs) points = 
    let
        x' = trunc x
        y' = trunc y
        dir' = trunc dir
        points' = Set.insert ((x',y'), dir') points
        discard = abs x > width/2 || abs y > height/2 || Set.member ((x', y'), dir') points
        isSingleton = xs == []
    in
        if
        | isSingleton && discard -> points
        | discard -> addNeighbors neighbors xs points
        | otherwise -> addNeighbors neighbors (xs ++ neighbors (x,y)) points'

defaultOrigin = ((0,0), 0)
largeNumber = 999999999

makeHexPattern : Float -> Float -> Pattern
makeHexPattern sideLen rotation = 
    let
        chordLen = sqrt ( 2*sideLen^2 * (1 - (cos <| exteriorAngle 6)) )
        angles = List.map (\x-> pi*x/3 - pi/6 + rotation) [1..6]

        neighbors : Point -> Pattern
        neighbors origin = List.map (\x -> (makePoint chordLen x origin, rotation) ) angles
    in
        Set.toList <| addNeighbors neighbors [((0,0),rotation)] Set.empty

makeSquarePattern: Float -> Float -> Pattern
makeSquarePattern sideLen rotation =
    let
        angles = List.map (\x-> pi*x/2 + rotation) [1..4]

        neighbors : Point -> Pattern
        neighbors origin = List.map (\x -> (makePoint sideLen x origin, rotation) ) angles
    in
        Set.toList <| addNeighbors neighbors [((0,0), rotation)] Set.empty

makeTrianglePattern: Float -> Float -> Pattern
makeTrianglePattern sideLen rotation =
    let
        angles = List.map (\x-> pi*x*2/3 + rotation) [1..3]

        neighbors : Point -> Pattern
        neighbors origin = List.map (\x -> (makePoint sideLen x origin, rotation) ) angles
    in
        Set.toList <| addNeighbors neighbors [((0,0), rotation)] Set.empty

opposite : Int -> Float -> Link
opposite n d = 
    let
        angle = 2 * pi / (toFloat n)
        initAngle = pi/2
        innerFunction : Link
        innerFunction side (x,y) =
            let angle' = (toFloat side) * angle + (initAngle)
            in ( (side + n//2) % n, (x + d*(cos angle'), y + d*(sin angle')))
    in
        innerFunction

makeHexLink size = opposite
                        6 
                        (sqrt ( 2*size^2 * (1 - (cos <| exteriorAngle 6)) ))
makeSquareLink size = opposite 4 size

--makeTriangleLink size = 

makeHexStamp : Float -> Float -> Stamp
makeHexStamp size rotation = (ngon 6 size, makeHexPattern size rotation)

makeSquareStamp : Float -> Float -> Stamp
makeSquareStamp size rotation = (ngon 4 size, makeSquarePattern size rotation)

makeTriangleStamp : Float -> Float -> Stamp
makeTriangleStamp size rotation = (ngon 3 size, makeTrianglePattern size rotation)

drawPolygon : Polygon -> Element
drawPolygon shape = 
    collage width height <| List.map ((traced (solid black)) << path) shape

drawStamp : Stamp -> Element
drawStamp (shape, pattern) = 
    let
        stamp : Form
        stamp = group <| List.map ((traced (solid black)) << path) shape
        points = List.map fst pattern
        dirs = List.map snd pattern
    in 
        collage width height 
            <| List.map (\f -> f stamp) 
            <| List.map (\x -> (move <| fst x) << (rotate <| snd x)) 
            <| pattern 

drawAll : Stamp -> Element 
drawAll (shape, pattern) = 
    let
        stamp : Form
        stamp = group <| List.map ((traced (solid black)) << path) shape
        points = List.map fst pattern
        dirs = List.map snd pattern
        stamps = 
            List.map (\f -> f stamp) 
                <| List.map (\x -> (move <| fst x) << (rotate <| snd x)) 
                <| pattern 
        line' = solid red
        line = { line' | width <- 3.0 }
    in 
        collage width height 
            <| stamps ++ (List.map ((traced line) << path) shape)
