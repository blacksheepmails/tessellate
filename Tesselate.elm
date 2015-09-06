import Graphics.Collage exposing (toForm, group, move, rotate, Form, Shape, path, polygon, rect, filled, outlined, traced, solid, collage)
import Graphics.Element exposing (image, layers, show, above, Element)
import Color exposing (red, black, toRgb, rgb)
import Set
import Mouse
import Signal exposing (merge)
import Html exposing (div)

type Action = MouseClick Int Int | None

type alias Dir = Float
type alias Point = (Float, Float)
type alias Edge = (Point, Point)
type alias Side = List Point
type alias Polygon = List Side
type alias Pattern = List (Point, Dir)
type alias Stamp = (Polygon, Pattern)
type alias Model = { stamp : Stamp,
                     editing : Bool,
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
        chordLen = (sqrt 2) * sideLen
        angles = List.map (\x-> pi*x/2 - pi/4 + rotation) [1..4]

        neighbors : Point -> Pattern
        neighbors origin = List.map (\x -> (makePoint chordLen x origin, rotation) ) angles
    in
        Set.toList <| addNeighbors neighbors [((0,0), rotation)] Set.empty

makeTrianglePattern: Float -> Float -> Pattern
makeTrianglePattern sideLen rotation =
    let
        angles = List.map (\x-> pi*x/3 + rotation) [1..6]

        neighbors : Point -> Pattern
        neighbors origin = List.map (\x -> (makePoint sideLen x origin, rotation) ) angles
    in
        Set.toList <| addNeighbors neighbors [((0,0), rotation)] Set.empty

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
    in collage width height <| List.map (\f -> f stamp) <| List.map (\x -> (move <| fst x) << (rotate <| snd x)) pattern 


clickSignal = Signal.map2 (\isDown (x,y) -> if isDown then MouseClick x y else None) Mouse.isDown Mouse.position

distSquared : Point -> Point -> Float
distSquared (x1,y1) (x2,y2) = (x1-x2)^2 - (y1-y2)^2

dist p1 p2 = sqrt <| distSquared p1 p2

(./) : Point -> Float -> Point
(./) (x,y) k = (x/k, y/k)
infixr 4 ./

proj v r = dot v r ./ (mag v)

mag (x,y) = sqrt (x^2+y^2)

dot (x1,y1) (x2,y2) = (x1*x2, y1*y2)

distPointEdge : Point -> Edge -> Float
distPointEdge (x,y) ((x1,y1),(x2,y2)) =
    let
        m = (y2-y1) / (x2-x1)
        -- two point form : y-y1=m(x-x1)
        -- cartesian form : 0 = mx - y + (-mx1 + y1)
        distFromLine = mag <| proj (m,1) (x-x1,y-y1)
        distBetweenEnds = dist (x1,y2) (x2,y2)
        parallelProj1 = mag <| proj (-1,m) (x1-x,y1-y)
        parallelProj2 = mag <| proj (-1,m) (x2-x,y2-y)
    in
        if
        | parallelProj1 > distBetweenEnds -> dist (x2,y2) (x,y)
        | parallelProj2 > distBetweenEnds -> dist (x1,y1) (x,y)
        | otherwise -> distFromLine
            
end (x::xs) = if xs == [] then x else end xs

getClosestSide : Point -> Polygon -> Int
getClosestSide (x,y) shape =
    let 
        indexedSides : List (Int, Side)
        indexedSides = List.indexedMap (\i x -> (i,x)) shape
        indexedClosest (i, side) (i', closest) = 
            let currDist = distPointEdge (x,y) (case List.head side of Just v -> v, (end side))
            in if currDist < closest 
                then (i, currDist) 
                else (i', closest)
    in
        fst <| List.foldl indexedClosest (0,largeNumber) indexedSides

insertPointInShape : Point -> Polygon -> Polygon
insertPointInShape p shape = 
    let i = getClosestSide p shape
    in insertPointInSide p (get i shape) :: (delete i shape)

delete : Int -> List a -> List a
delete i xs = (List.take i xs) ++ (List.drop (i+1) xs)

get : Int -> List a -> a
get i (x::xs) = if i == 0 then x else get (i-1) xs

insertPointInSide : Point -> Side -> Side
insertPointInSide p side = 
    let
        squareDists = List.map (distSquared p) side
        squareDistSums = List.map fst <| List.scanl (\dist (_, prevDist) -> (dist+prevDist,dist)) (largeNumber, case List.head squareDists of Just v -> v) squareDists
        ((minDist, index), _) = List.foldl 
                                    (\dist ((minDist,c), counter) -> (if dist < minDist 
                                                                        then (dist,counter) 
                                                                        else (minDist,c), counter+1)) 
                                    ((largeNumber, -1), -1)
                                    squareDistSums
    in
        (List.take index side) ++ [p] ++ (List.drop index side)

insertShapeInStamp : Polygon -> Stamp -> Stamp
insertShapeInStamp shape (s, p) = (shape, p)

update : Action -> Model -> Model
update action model = 
    let shape = fst model.stamp
    in case action of 
      None -> model
      MouseClick x y -> addDebug (toCollageCoords x y) 
                        { model | stamp <- insertShapeInStamp 
                                            (insertPointInShape (toCollageCoords x y) shape)
                                            model.stamp
                        }
      otherwise -> model

addDebug : Point -> Model -> Model
addDebug point model = {model | debug <- toString point}

toCollageCoords : Int -> Int -> (Float, Float)
toCollageCoords x y = 
    (
        toFloat <| x - width//2,
        toFloat <| height//2 - y
    )

model = {stamp = makeSquareStamp 50 (pi/4),
         editing = False,
         debug = ""}

drawModel model = div [] 
        [ Html.fromElement <|
            layers 
                [ if model.editing == False then drawStamp model.stamp else drawPolygon <| fst model.stamp
                , show model.debug ]
        ]

main = Signal.map drawModel <| Signal.foldp update model clickSignal