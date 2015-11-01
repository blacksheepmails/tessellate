import Graphics.Collage exposing (toForm, group, move, rotate, Form, Shape, path, polygon, rect, filled, outlined, traced, solid, collage)
import Graphics.Element exposing (image, layers, show, above, Element)
import Color exposing (red, black, toRgb, rgb)
import Set
import Mouse
import Signal exposing (merge)
import Html exposing (div, button, text, b, Html)
import Html.Attributes exposing (id)
import Html.Events exposing (onClick)
import Stamps exposing (..)
import Util exposing (..)
import Dict

type Action = Drag Int Int | MoveMouse Int Int | SelectShape String | SelectPattern Int | None
type alias Model = { stamp : Stamp,
                     editing : Bool,
                     lastPoint: (Int,Int),
                     shape: String,
                     pattern: Int,
                     debug : String }

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
      Drag x y -> updateLastPoint x y
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
      SelectShape shape -> {model | shape <- shape, pattern <- -1}
      SelectPattern pattern_index -> {model | pattern <- pattern_index, 
                                              stamp <- snd <| get pattern_index <| case Dict.get model.shape stampDict of Just v -> v}
      otherwise -> model


stampDict : Dict.Dict String (List (String, Stamp))
stampDict = Dict.fromList [ ("Triangle", [("only triangle pattern", makeTriangleStamp 100 0)])
                          , ("Square", [ ("parallel sides linked", makeSquareStamp 70 0)
                                       , ("adjacent sides linked", makeSquare2Stamp 70 0)])
                          , ("Hexagon", [ ("parallel sides linked", makeHexStamp 50 0)
                                        , ("adjacent sides linked", makeHex2Stamp 50 0)]) ]

addDebug : String -> Model -> Model
addDebug msg model = {model | debug <- msg }

replaceStamp : Stamp -> Model -> Model
replaceStamp stamp model = {model | stamp <- stamp}

model : Model
model = {stamp = emptyStamp,--(pi/4),
         editing = False,
         lastPoint = (0, 0),
         shape = "",
         pattern = -1,
         debug = ""}

drawTesselation : Model -> Html
drawTesselation model = div [] 
        [ Html.fromElement <|
            layers 
                [ drawAll model.stamp
                , show model.debug ]
        ]

drawSelectors : Signal.Address Action -> Signal.Address Action -> Model -> Html
drawSelectors shapeAddress patternAddress model = 
    let 
        formatSelected option selected = formatSelectedDescription option selected option
        formatSelectedDescription option selected desc = if option == selected 
                                then b [] [text desc]
                                else text desc                        
    in 
        div [] 
            [ text "select shape, then select tesselation pattern. idc if the tesselation pattern has no descriptions."
            , div [id "shape-select"] 
                [
                    button 
                        [onClick shapeAddress (SelectShape "Triangle") ]
                        [formatSelected "Triangle" model.shape],
                    button
                        [onClick shapeAddress (SelectShape "Square")]
                        [formatSelected "Square" model.shape],
                    button
                        [onClick shapeAddress (SelectShape "Hexagon")]
                        [formatSelected "Hexagon" model.shape]
                ]
            , if model.shape /= ""
                then div [] <| List.indexedMap 
                                (\i (description, stamp) -> button 
                                                [onClick patternAddress (SelectPattern i)]
                                                [formatSelectedDescription (toString i) (toString model.pattern) description]
                                )
                                (case Dict.get model.shape stampDict of Just v -> v)
                else
                    div [] []
            ]

drawPage : Signal.Address Action -> Signal.Address Action -> Model -> Html
drawPage shapeAddress patternAddress model = div []
    [ drawTesselation model,
      drawSelectors shapeAddress patternAddress model
    ]

shapeSelectMailbox = Signal.mailbox None
patternSelectMailbox = Signal.mailbox None

mouseSignal = Signal.map2 (\isDown (x,y) -> if isDown && (inCanvas x y) then Drag x y else MoveMouse x y) Mouse.isDown Mouse.position

main = Signal.map (drawPage shapeSelectMailbox.address patternSelectMailbox.address) 
        <| Signal.foldp update model 
        <| Signal.mergeMany [mouseSignal, shapeSelectMailbox.signal, patternSelectMailbox.signal]