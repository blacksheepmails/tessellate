module Stamps where

import Graphics.Collage exposing (toForm, group, move, rotate, Form, Shape, path, polygon, rect, filled, outlined, traced, solid, collage)
import Graphics.Element exposing (image, layers, show, above, Element)
import Color exposing (red, black, toRgb, rgb)
import Set
import List as List
import Util exposing (..)


width = 1000
height = 500

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

addNeighbors : ((Point,Dir,Int) -> Pattern) -> List (Point, Dir,Int) -> Set.Set (Point, Dir,Int) -> Set.Set (Point, Dir,Int)
addNeighbors neighbors (((x,y),dir,flipped)::xs) points = 
    let
        x' = trunc x
        y' = trunc y
        dir' = trunc dir
        points' = Set.insert ((x',y'), dir',flipped) points
        discard = abs x > width/2 || abs y > height/2 || Set.member ((x', y'), dir',flipped) points
        isSingleton = xs == []
    in
        if
        | isSingleton && discard -> points
        | discard -> addNeighbors neighbors xs points
        | otherwise -> addNeighbors neighbors (xs ++ neighbors ((x,y),dir,flipped)) points'

largeNumber = 999999999

makeHexPattern : Float -> Float -> Pattern
makeHexPattern sideLen rotation = 
    let
        chordLen = sqrt ( 2*sideLen^2 * (1 - (cos <| exteriorAngle 6)) )
        angles = List.map (\x-> pi*x/3 - pi/6 + rotation) [1..6]

        neighbors : (Point, Dir, Int) -> Pattern
        neighbors (origin,_,_) = List.map (\x -> (makePoint chordLen x origin, rotation, 0) ) angles
    in
        Set.toList <| addNeighbors neighbors [((0,0), rotation, 0)] Set.empty

makeSquarePattern: Float -> Float -> Pattern
makeSquarePattern sideLen rotation =
    let
        angles = List.map (\x-> pi*x/2 + rotation) [1..4]

        neighbors : (Point, Dir, Int) -> Pattern
        neighbors (origin,_,_) = List.map (\x -> (makePoint sideLen x origin, rotation, 0) ) angles
    in
        Set.toList <| addNeighbors neighbors [((0,0), rotation, 0)] Set.empty

makeTrianglePattern: Float -> Dir -> Pattern
makeTrianglePattern sideLen rotation =
    let
        angles = List.map (\x-> pi*x*2/3 + rotation) [1..3]
        orient (x::y::z::_) = [(x,1), (y,1), (z,0)]
        neighbors : (Point, Dir, Int) -> Pattern
        neighbors ((x,y),rotation',flipped') = List.map (\(angle, flipped) -> (makePoint sideLen angle (x,y), rotation, (flipped+flipped')%2) ) <| orient angles
    in
        Set.toList <| addNeighbors neighbors [((0,0), rotation, 0)] Set.empty
        --neighbors ((0,0), rotation, 0)

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

makeHexLink : Float -> Link
makeHexLink size = opposite
                        6 
                        (sqrt ( 2*size^2 * (1 - (cos <| exteriorAngle 6)) ))

makeSquareLink : Float -> Link
makeSquareLink size = opposite 4 size

makeTriangleLink : Polygon -> Link
makeTriangleLink shape = 
    let
        innerFunction : Link
        innerFunction side p = if side == 0 
            then
                let (s,e) = sideToEdge <| get 0 shape
                in (0, fromRelativeCoords (toRelativeCoords p (s,e)) (e,s) )
            else
                let 
                    side' = (3 - side) % 3
                    edge = sideToEdge <| get side shape
                    edge' = sideToEdge <| get side' shape
                in 
                    (side', fromRelativeCoords (negateY <| toRelativeCoords p edge) edge')
    in
        innerFunction

makeHexStamp : Float -> Float -> Stamp
makeHexStamp size rotation = 
    let 
        shape = ngon 6 size
        pattern = makeHexPattern size rotation
        link = makeHexLink size
    in
        {shape = shape, pattern = pattern, link = link}

makeSquareStamp : Float -> Float -> Stamp
makeSquareStamp size rotation = 
    let
        shape = ngon 4 size
        pattern = makeSquarePattern size rotation
        link = makeSquareLink size
    in
        {shape = shape, pattern = pattern, link = link}

makeTriangleStamp : Float -> Float -> Stamp
makeTriangleStamp size rotation = 
    let 
        shape = ngon 3 size
        pattern = makeTrianglePattern size rotation
        link = makeTriangleLink shape
    in 
        {shape = shape, pattern = pattern, link = link}

drawPolygon : Polygon -> List Form
drawPolygon shape = List.map ((traced (solid black)) << path) shape

drawStamp : Stamp -> List Form
drawStamp stamp = 
    let
        form : Form
        form = group <| List.map ((traced (solid red)) << path) stamp.shape
        origin = (\(x,_,_) -> x)
        dir = (\(_,x,_) -> x)
        xflipped = (\(_,_,x) -> x)
    in 
        List.map (\f -> f form) 
        <| List.map (\x -> (rotate <| dir x) << (move <| origin x) << (if xflipped x == 1 then flipX else identity)) stamp.pattern 

draw : List Form -> Element
draw forms = collage width height forms

drawAll : Stamp -> Element 
drawAll stamp = draw <| (drawStamp stamp) ++ (drawPolygon stamp.shape)
