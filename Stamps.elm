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

drawPolygon : Polygon -> Element
drawPolygon shape = 
    collage width height <| List.map ((traced (solid black)) << path) shape

drawStamp : Stamp -> Element
drawStamp stamp = 
    let
        form : Form
        form = group <| List.map ((traced (solid black)) << path) stamp.shape
        points = List.map fst stamp.pattern
        dirs = List.map snd stamp.pattern
    in collage width height <| List.map (\f -> f form) <| List.map (\x -> (move <| fst x) << (rotate <| snd x)) stamp.pattern 

