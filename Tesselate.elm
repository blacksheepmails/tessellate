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
import Keyboard


--Triangles should have just 1: adj flipped. adj regular does not provide tiling
--Squares should have at least 5. maybe 6: opp, adj, opflip, adjflip, opflip1, and adjflip1?
--Hexagons should have idk. should at least implement opp adj (adjflip opflip cant exist perhaps need to flip even number of times) , skip1 cant exist. variatons of skip1 and adj? dont think so either.

type Action = NewPoint Int Int | Drag Int Int | SelectShape String | SelectPattern Int | None
type alias Model = { stamp : Stamp,
                     editing : Bool,
                     shape: String,
                     pattern: Int,
                     debug : String }


getClosestSide : Point -> Polygon -> Int
getClosestSide (x,y) shape =
    let 
        indexedEdges : List (Int, Edge)
        indexedEdges = List.concat <| List.indexedMap (\i side -> mapBetween (\x y -> (i,(x,y))) side) shape

        indexedClosest (i, edge) (i', closest) = 
            let currDist = distPointEdge (x,y) edge
            in if currDist < closest 
                then (i, currDist) 
                else (i', closest)
    in
        fst <| List.foldl indexedClosest (0,largeNumber) indexedEdges

getClosestPoint : Point -> Polygon -> Maybe (Int, Int)
getClosestPoint p sides = 
    case minimumBy 
        (distSquared p << fst)
        <| List.concat <| List.indexedMap (\i side -> List.map (\p' -> (p', i)) (middleList side)) sides
        of
            Just (point, sideIndex) -> Just (getIndexOf point (get sideIndex sides), sideIndex)
            Nothing -> Nothing

replacePointInStamp: Point -> Stamp -> Stamp --need last side thing so dragged thing doesnt skip around!!!!!!!!!!!!!!!!!!!!! maybe add point ids to polygons?
replacePointInStamp p stamp = 
    case getClosestPoint p stamp.shape of
        Just (pointIndex, sideIndex) -> 
            let 
                side = get sideIndex stamp.shape
                (sideIndex', p') = stamp.link sideIndex p
                side' = get sideIndex' stamp.shape
                pointIndex' = case getClosestPoint p' [side'] of Just (pi, si) -> pi
                replacePointInSide : Int -> Int -> Point -> List Side -> List Side
                replacePointInSide si pi point shape = replaceList si (replaceList pi point <| get si shape) shape
            in 
                {stamp | shape <- stamp.shape
                                    |> replacePointInSide sideIndex pointIndex p 
                                    |> replacePointInSide sideIndex' pointIndex' p'}
        Nothing -> stamp

insertPointInStamp : Point -> Stamp -> Stamp --something is still a bit funcky but it doesnt happen often
insertPointInStamp point stamp = 
    let
        insertPointInSide : Point -> Side -> Side
        insertPointInSide p side = 
            let
                edges = mapBetween (\p1 p2 -> (p1,p2)) side 
                distToEdges = List.indexedMap (\i edge -> (distPointEdge p edge, i)) edges
                mins = List.take 2 <| List.sortBy fst distToEdges
                calcIndex x = snd x + 1
                tiebreaker i1 i2 = 
                    let
                        (s1,e1) = get i1 edges 
                        (s2,e2) = get i2 edges
                        distSquared1 = (distSquared s1 p + distSquared e1 p)
                        distSquared2 = (distSquared s2 p + distSquared e2 p)
                    in
                        if distSquared1 < distSquared2 then i1+1 else i2+1
                index = case mins of
                    x::y::[] -> if fuzzyEquals (fst x) (fst y) 
                                    then tiebreaker (snd x) (snd y)
                                    else calcIndex x
                    x::[] -> calcIndex x
            in
                insertList index p side
        i = getClosestSide point stamp.shape
        (i',point') = stamp.link i point
        shape' : Polygon
        shape' = replaceList i' (insertPointInSide point' (get i' stamp.shape)) stamp.shape
    in
        {stamp | shape <- (replaceList i (insertPointInSide point (get i shape')) shape') }


update : Action -> Model -> Model
update action model = 
    case action of 
      None -> model
      NewPoint x y -> { model | stamp <- insertPointInStamp (toCollageCoords x y) model.stamp }
      Drag x y -> { model | stamp <- replacePointInStamp (toCollageCoords x y) model.stamp }
      SelectShape shape -> {model | shape <- shape, pattern <- -1}
      SelectPattern pattern_index -> {model | pattern <- pattern_index, 
                                              stamp <- snd <| get pattern_index <| case Dict.get model.shape stampDict of Just v -> v}
      otherwise -> model

stampDict : Dict.Dict String (List (String, Stamp))
stampDict = Dict.fromList [ ("Triangle", [("only triangle pattern (p2gg)", makeTriangleStamp 100 0)])
                          , ("Square", [ ("parallel sides linked (p1)", makeSquareStamp 70 0)
                                       , ("adjacent sides linked (p4)", makeSquare2Stamp 70 0)
                                       , ("opposite sides fliped (p2gg)", makeSquare3Stamp 70 0)
                                       , ("adjacent sides flipped (p1)", makeSquare4Stamp 70 0)
                                       , ("opposite flipped 1 pair (p2gg)", makeSquare5Stamp 70 0)])
                          , ("Hexagon", [ ("parallel sides linked (p1)" , makeHexStamp 50 0)
                                        , ("adjacent sides linked (p3)", makeHex2Stamp 50 0)]) ]

addDebug : String -> Model -> Model
addDebug msg model = {model | debug <- msg }

replaceStamp : Stamp -> Model -> Model
replaceStamp stamp model = {model | stamp <- stamp}

model : Model
model = {stamp = emptyStamp,
         editing = False,
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
            [ text "select shape, then select tesselation pattern. the base shape is black and in the center. to edit: ctrl-click to add new point, drag to edit existing point. if you ctrl-drag, you'll be creating lots and lots of points! in parens are the associated wallpaper groups the generated pattern will produce."
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

drawSignal = Signal.map3
                (\mouseDown (x,y) ctrl -> if mouseDown && (inCanvas x y) 
                                            then if ctrl then NewPoint x y else Drag x y
                                            else None)
                Mouse.isDown Mouse.position Keyboard.ctrl

main = Signal.map (drawPage shapeSelectMailbox.address patternSelectMailbox.address) 
        <| Signal.foldp update model 
        <| Signal.mergeMany [drawSignal, shapeSelectMailbox.signal, patternSelectMailbox.signal]
