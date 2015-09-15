import Graphics.Collage exposing (toForm, group, move, rotate, Form, Shape, path, polygon, rect, filled, outlined, traced, solid, collage)
import Graphics.Element exposing (image, layers, show, above, Element)
import Color exposing (red, black, toRgb, rgb)
import Set
import Mouse
import Signal exposing (merge)
import Html exposing (div)
import Stamps exposing (..)

type Action = Drag Int Int | MoveMouse Int Int | None

distSquared : Point -> Point -> Float
distSquared (x1,y1) (x2,y2) = (x1-x2)^2 + (y1-y2)^2

replaceList i v l = (List.take i l) ++ [v] ++ (List.drop (i+1) l)


dist p1 p2 = sqrt <| distSquared p1 p2

(./) : Point -> Float -> Point
(./) (x,y) k = (x/k, y/k)
infixr 4 ./

proj v r = dot v r / (mag v)

mag (x,y) = sqrt (x^2+y^2)

dot (x1,y1) (x2,y2) = x1*x2 + y1*y2

distPointEdge : Point -> Edge -> Float
distPointEdge (x,y) ((x1,y1),(x2,y2)) =
    let
        edge = (x2-x1, y2-y1)
        (xEdge, yEdge) = edge
        -- two point form : y-y1=m(x-x1)
        -- cartesian form : 0 = mx - y + (-mx1 + y1)
        distFromLine = abs <| proj (-yEdge, xEdge) (x-x1,y-y1)
        distBetweenEnds = dist (x1,y1) (x2,y2)
        parallelProj1 = abs <| proj edge (x1-x,y1-y)
        parallelProj2 = abs <| proj edge (x2-x,y2-y)
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

insertPointInSide : Point -> Side -> Side
insertPointInSide p side = 
    let
        squareDists = List.map (distSquared p) side
        squareDistSums = mapBetween (+) squareDists
        index = 1 + (getIndexOf (case List.minimum squareDistSums of Just v -> v) squareDistSums)
    in
        (List.take index side) ++ [p] ++ (List.drop index side)

replacePointInSide : Point -> Side -> Side
replacePointInSide p side = 
    let
        squareDists = List.map (distSquared p) side
        index = getIndexOf (case List.minimum squareDists of Just v -> v) squareDists
    in
        if index == 0 || index == (List.length side - 1)
            then side
            else (List.take index side) ++ [p] ++ (List.drop (index+1) side)

insertShapeInStamp : Polygon -> Stamp -> Stamp
insertShapeInStamp shape (s, p) = (shape, p)

updateStamp : Point -> (Point -> Side -> Side) -> Link -> Stamp -> Stamp
updateStamp point f link stamp = 
    let
        shape = fst stamp
        i = getClosestSide point shape
        (i',point') = link i point
    in
        insertShapeInStamp
            (replaceList i (f point (get i shape)) 
                <| replaceList i' (f point' (get i' shape)) shape)
            stamp

updateLastPoint : Int -> Int -> Model -> Model
updateLastPoint x y model = { model | lastPoint <- (x,y)}

update : Action -> Model -> Model
update action model = 
    case action of 
      None -> model
      MoveMouse x y -> updateLastPoint x y model
      Drag x y -> updateLastPoint x y <|
                    if model.lastPoint == (x,y) 
                        then
                            addDebug (toCollageCoords x y)
                            { model | stamp <- updateStamp
                                                    (toCollageCoords x y) 
                                                    insertPointInSide
                                                    model.link
                                                    model.stamp
                            }
                        else 
                            addDebug (toCollageCoords x y)
                            { model | stamp <- updateStamp 
                                                    (toCollageCoords x y)
                                                    replacePointInSide
                                                    model.link
                                                    model.stamp
                            }
      otherwise -> model

addDebug : Point -> Model -> Model
addDebug point model = {model | debug <- (toString point) 
                                      ++ (toString <| insertPointInSide (5, 9) [(1,3), (2,4), (5,5)])
                        }

toCollageCoords : Int -> Int -> (Float, Float)
toCollageCoords x y = 
    (
        toFloat <| x - width//2,
        toFloat <| height//2 - y
    )

model : Model
model = {stamp = makeSquareStamp 150 0, --(pi/4),
         link = makeSquareLink 150,
         editing = False,
         lastPoint = (0, 0),
         debug = ""}

drawModel model = div [] 
        [ Html.fromElement <|
            layers 
                [ if model.editing == False then drawStamp model.stamp else drawPolygon <| fst model.stamp
                , show model.debug ]
        ]


mouseSignal = Signal.map2 (\isDown (x,y) -> if isDown then Drag x y else MoveMouse x y) Mouse.isDown Mouse.position

main = Signal.map drawModel <| Signal.foldp update model mouseSignal