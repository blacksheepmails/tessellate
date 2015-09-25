import Graphics.Collage exposing (toForm, group, move, rotate, Form, Shape, path, polygon, rect, filled, outlined, traced, solid, collage)
import Graphics.Element exposing (image, layers, show, above, Element)
import Color exposing (red, black, toRgb, rgb)
import Set
import Mouse
import Signal exposing (merge)
import Html exposing (div)
import Stamps exposing (..)
import Util exposing (..)

type Action = Drag Int Int | MoveMouse Int Int | None


distPointEdge : Point -> Edge -> Float
distPointEdge (x,y) ((x1,y1),(x2,y2)) =
    let
        edge = (x2-x1, y2-y1)
        (xEdge, yEdge) = edge
        distFromLine = abs <| proj (-yEdge, xEdge) (x-x1,y-y1)
        distBetweenEnds = dist (x1,y1) (x2,y2)
        parallelProj1 = abs <| proj edge (x1-x,y1-y)
        parallelProj2 = abs <| proj edge (x2-x,y2-y)
    in
        if
        | parallelProj1 > distBetweenEnds -> dist (x2,y2) (x,y)
        | parallelProj2 > distBetweenEnds -> dist (x1,y1) (x,y)
        | otherwise -> distFromLine


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
insertShapeInStamp shape stamp = {stamp | shape <- shape}

updateStamp : Point -> (Point -> Side -> Side) -> Stamp -> Stamp
updateStamp point f stamp = 
    let
        i = getClosestSide point stamp.shape
        (i',point') = stamp.link i point
        shape' : Polygon
        shape' = replaceList i' (f point' (get i' stamp.shape)) stamp.shape
    in
        insertShapeInStamp
            ( replaceList i (f point (get i shape')) shape' )
            stamp

updateLastPoint : Int -> Int -> Model -> Model
updateLastPoint x y model = { model | lastPoint <- (x,y)}

update : Action -> Model -> Model
update action model = 
    case action of 
      None -> model
      MoveMouse x y -> updateLastPoint x y model
      Drag x y -> addDebug (toString <| adjacent <| ngon 4 10)
                    <| updateLastPoint x y
                    <| if model.lastPoint == (x,y) 
                        then
                            { model | stamp <- updateStamp
                                                    (toCollageCoords x y) 
                                                    insertPointInSide
                                                    model.stamp
                            }
                        else 
                            { model | stamp <- updateStamp 
                                                    (toCollageCoords x y)
                                                    replacePointInSide
                                                    model.stamp
                            }
      otherwise -> model

addDebug : String -> Model -> Model
addDebug msg model = {model | debug <- msg }

toCollageCoords : Int -> Int -> (Float, Float)
toCollageCoords x y = 
    (
        toFloat <| x - width//2,
        toFloat <| height//2 - y
    )

model : Model
model = {stamp = makeHex2Stamp 50 0,--(pi/4),
         editing = False,
         lastPoint = (0, 0),
         debug = ""}

drawModel model = div [] 
        [ Html.fromElement <|
            layers 
                --[ draw <| if model.editing == False then drawStamp model.stamp else drawPolygon model.stamp.shape
                [ drawAll model.stamp
                , show model.debug ]
        ]


mouseSignal = Signal.map2 (\isDown (x,y) -> if isDown then Drag x y else MoveMouse x y) Mouse.isDown Mouse.position

main = Signal.map drawModel <| Signal.foldp update model mouseSignal
