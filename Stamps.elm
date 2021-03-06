module Stamps where

import Graphics.Collage exposing (toForm, group, move, rotate, Form, Shape, path, polygon, rect, filled, outlined, traced, solid, collage)
import Graphics.Element exposing (image, layers, show, above, Element)
import Color exposing (red, black, toRgb, rgb)
import Set
import List as List
import Util exposing (..)


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

makeHexPattern : Float -> Float -> Pattern
makeHexPattern sideLen rotation = 
    let
        chordLen = sqrt ( 2*sideLen^2 * (1 - (cos <| exteriorAngle 6)) )
        angles = List.map (\x-> 2*pi*x/3 - pi/6 + rotation) [1..3]

        neighbors : (Point, Dir, Int) -> Pattern
        neighbors (origin,_,_) = List.map (\x -> (makePoint chordLen x origin, rotation, 0) ) angles
    in
        Set.toList <| addNeighbors neighbors [((0,0), rotation, 0)] Set.empty

makeHex2Pattern : Float -> Float -> Pattern
makeHex2Pattern sideLen rotation = 
    let
        chordLen = sqrt ( 2*sideLen^2 * (1 - (cos <| exteriorAngle 6)) )
        angles = List.map (\x-> 2*pi*x/3 - pi/6 + rotation) [1..3]

        neighbors : (Point, Dir, Int) -> Pattern
        neighbors (origin,dir,_) = List.map (\x -> (makePoint chordLen x (origin .+ (makePoint chordLen (dir+pi/6) (0,0)) ), rem2pi (rotation+dir+2*pi/3), 0) ) angles
    in
        Set.toList <| addNeighbors neighbors [((0,0), rotation, 0)] Set.empty
        --neighbors ((0,0), rotation, 0)

makeHex3Pattern : Float -> Float -> Pattern
makeHex3Pattern sideLen rotation = 
    let
        chordLen = sqrt ( 2*sideLen^2 * (1 - (cos <| exteriorAngle 6)) )
        angles = List.map (\x-> 2*pi*x/3 - pi/6 + rotation) [1..3]

        neighbors : (Point, Dir, Int) -> Pattern
        neighbors (origin,dir,_) = List.map (\x -> (makePoint chordLen x (origin .+ (makePoint chordLen (dir+pi/6) (0,0)) ), rem2pi (rotation+dir+2*pi/3), 0) ) angles
    in
        Set.toList <| addNeighbors neighbors [((0,0), rotation, 0)] Set.empty
        --neighbors ((0,0), rotation, 0)

makeSquarePattern: Float -> Float -> Pattern
makeSquarePattern sideLen rotation =
    let
        angles = List.map (\x-> pi*x/2 + rotation) [1..4]

        neighbors : (Point, Dir, Int) -> Pattern
        neighbors (origin,_,_) = List.map (\x -> (makePoint sideLen x origin, rotation, 0) ) angles
    in
        Set.toList <| addNeighbors neighbors [((0,0), rotation, 0)] Set.empty

makeSquare2Pattern: Float -> Float -> Pattern
makeSquare2Pattern sideLen rotation =
    let
        angles = List.map (\x-> pi*x/2 + pi/4 + rotation) [1..4]
        chordLen = sideLen * sqrt(2)
        neighbors : (Point, Dir, Int) -> Pattern
        neighbors (origin,dir,_) = List.map (\x -> (makePoint chordLen x (origin .+ (makePoint chordLen (dir+pi/4) (0,0) )), rem2pi (dir+rotation+pi), 0) ) angles
    in
        Set.toList <| addNeighbors neighbors [((0,0), rotation, 0)] Set.empty

makeSquare3Pattern: Float -> Float -> Pattern
makeSquare3Pattern sideLen rotation =
    let
        angles = List.map (\x-> pi*x/2 + pi/4 + rotation) [1..4]
        chordLen = sideLen * sqrt(2)
        neighbors : (Point, Dir, Int) -> Pattern
        neighbors (origin,dir,_) = List.map (\x -> (makePoint chordLen x (origin .+ (makePoint chordLen (dir+pi/4) (0,0) )), rem2pi (rotation+dir+pi), 0) ) angles
    in
        Set.toList <| addNeighbors neighbors [((0,0), rotation, 0)] Set.empty

makeSquare4Pattern: Float -> Float -> Pattern
makeSquare4Pattern sideLen rotation =
    let
        angles = List.map (\x-> pi*x/2 + pi/4 + rotation) [1..4]
        chordLen = sideLen * sqrt(2)
        neighbors : (Point, Dir, Int) -> Pattern
        neighbors (origin,_,_) = List.map (\x -> (makePoint chordLen x (origin .+ (makePoint chordLen (pi/4) (-sideLen,-sideLen) )), rotation, 0) ) angles
    in
        Set.toList <| addNeighbors neighbors [((0,0), rotation, 0)] Set.empty

makeSquare5Pattern: Float -> Float -> Pattern
makeSquare5Pattern sideLen rotation =
    let
        angles = List.map (\x-> pi*x/2 + pi/4 + rotation) [1..4]
        chordLen = sideLen * sqrt(2)
        neighbors : (Point, Dir, Int) -> Pattern
        neighbors (origin,_,flipped) = List.map (\x -> (makePoint chordLen x (if flipped ==1 then origin else origin .+ (makePoint sideLen pi (0,0) )), rotation, (flipped+1)%2) ) angles

    in
        Set.toList <| addNeighbors neighbors [((0,0), rotation, 0)] Set.empty
        --neighbors ((0,0), rotation, 0)


makeTrianglePattern: Float -> Dir -> Pattern
makeTrianglePattern sideLen rotation =
    let
        angles = List.map (\x-> pi*x*2/3 + rotation) [1..3]
        orient (x::y::z::_) = [(x,1), (y,1), (z,0)]
        neighbors : (Point, Dir, Int) -> Pattern
        neighbors ((x,y),rotation',flipped') = List.map (\(angle, flipped) -> (makePoint sideLen angle (x,y), rotation, (flipped+flipped')%2) ) <| orient angles
    in
        Set.toList <| addNeighbors neighbors [((0,0), rotation, 0)] Set.empty


opposite : Polygon -> Link
opposite shape = 
    let
        n = List.length shape
        innerFunction : Link
        innerFunction side p =
            let 
                side' = (side+(n//2)) % n
                (s,e) = sideToEdge <| get side shape
                (s',e') = sideToEdge <| get side' shape
            in (side' , fromRelativeCoords (toRelativeCoords p (s,e)) (e',s') )
    in
        innerFunction

oppositeFlip : Polygon -> Link
oppositeFlip shape = 
    let
        n = List.length shape
        innerFunction : Link
        innerFunction side p =
            let 
                side' = (side+(n//2)) % n
                (s,e) = sideToEdge <| get side shape
                (s',e') = sideToEdge <| get side' shape
            in (side' , fromRelativeCoords (negateY (toRelativeCoords p (s,e))) (s',e') )
    in
        innerFunction

oppositeFlip1 : Polygon -> Link
oppositeFlip1 shape =
    let
        n = List.length shape
        innerFunction : Link
        innerFunction side p =
            let 
                side' = (side+(n//2)) % n
                (s,e) = sideToEdge <| get side shape
                (s',e') = sideToEdge <| get side' shape
            in 
                if side == 0 || side' == 0
                    then (side' , fromRelativeCoords (negateY (toRelativeCoords p (s,e))) (s',e') )
                    else (side' , fromRelativeCoords (toRelativeCoords p (s,e)) (e',s') )
    in
        innerFunction

adjacent : Polygon -> Link
adjacent shape =
    let
        n = List.length shape
        innerFunction : Link
        innerFunction side p =
            let 
                side' = (if odd side then side+1 else side-1) % n
                (s,e) = sideToEdge <| get side shape
                (s',e') = sideToEdge <| get side' shape
            in (side' , fromRelativeCoords (toRelativeCoords p (s,e)) (e',s') )
    in
        innerFunction

adjacentFlip : Polygon -> Link
adjacentFlip shape =
    let
        n = List.length shape
        innerFunction : Link
        innerFunction side p =
            let 
                side' = (if odd side then side+1 else side-1) % n
                (s,e) = sideToEdge <| get side shape
                (s',e') = sideToEdge <| get side' shape
            in (side' , fromRelativeCoords (negateY (toRelativeCoords p (s,e))) (s',e') )
    in
        innerFunction

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
        link = opposite shape
    in
        {shape = shape, pattern = pattern, link = link}

makeHex2Stamp : Float -> Float -> Stamp
makeHex2Stamp size rotation = 
    let 
        shape = ngon 6 size
        pattern = makeHex2Pattern size rotation
        link = adjacent shape
    in
        {shape = shape, pattern = pattern, link = link}


makeSquareStamp : Float -> Float -> Stamp
makeSquareStamp size rotation = 
    let
        shape = ngon 4 size
        pattern = makeSquarePattern size rotation
        link = opposite shape
    in
        {shape = shape, pattern = pattern, link = link}

makeSquare2Stamp : Float -> Float -> Stamp
makeSquare2Stamp size rotation =
    let
        shape = ngon 4 size
        pattern = makeSquare2Pattern size rotation
        link = adjacent shape
    in
        {shape = shape, pattern = pattern, link = link}

makeSquare3Stamp : Float -> Float -> Stamp
makeSquare3Stamp size rotation =
    let
        shape = ngon 4 size
        pattern = makeSquare3Pattern size rotation
        link = oppositeFlip shape
    in
        {shape = shape, pattern = pattern, link = link}

makeSquare4Stamp : Float -> Float -> Stamp
makeSquare4Stamp size rotation =
    let
        shape = ngon 4 size
        pattern = makeSquare4Pattern size rotation
        link = adjacentFlip shape
    in
        {shape = shape, pattern = pattern, link = link}

makeSquare5Stamp : Float -> Float -> Stamp
makeSquare5Stamp size rotation =
    let
        shape = ngon 4 size
        pattern = makeSquare5Pattern size rotation
        link = oppositeFlip1 shape
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

emptyStamp : Stamp
emptyStamp = { 
                shape = ngon 0 0,
                pattern = [],
                link = (\int point -> (0,(0,0)))
             }

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
