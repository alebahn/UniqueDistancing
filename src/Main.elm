module Main exposing (..)

import Browser
import String exposing (fromInt, toInt)
import Html exposing (Html, button, div, label, input, text, table, tr, td)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick, onInput)
import Array exposing (Array, fromList, toList)
import Maybe exposing (withDefault)


-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type alias Point =
  { x : Int
  , y : Int
  }

type alias Model = 
  { size : Int
  , markers : List Point
  }

makeModel : Int -> Model
makeModel size = { size = size
                 , markers = [
                               { x = 0
                               , y = 0
                               }
                             ]
                 }

init : Model
init = makeModel 6

-- GRID LOGIC AND SOLVING

type alias Bounds =
  { left : Int
  , right : Int
  , top : Int
  , bottom : Int
  }

getBounds : Model -> Bounds
getBounds model =
  let
    xs = List.map .x model.markers
    ys = List.map .y model.markers
    minX = withDefault 0 (List.minimum xs)
    maxX = withDefault 0 (List.maximum xs)
    minY = withDefault 0 (List.minimum ys)
    maxY = withDefault 0 (List.maximum ys)
    width = maxX - minX + 1
    height = maxY - minY + 1
    xOffset = model.size - width
    yOffset = model.size - height
  in
    { left = minX - xOffset
    , right = maxX + xOffset
    , bottom = minY - yOffset
    , top = maxY + yOffset
    }

type CellStatus = Empty | Invalid | Marker

type alias Cell =
  { coordinates : Point
  , status : CellStatus
  }

type alias Grid = Array (Array Cell)

--actualy distance squared
getDistance : Point -> Point -> Int
getDistance pointA pointB = (pointA.x - pointB.x)^2 + (pointA.y - pointB.y)^2

--actually distance squared
getDistances : List Point -> List Int
getDistances markers =
  case markers of
    [] -> []
    marker :: rest -> List.map (getDistance marker) rest ++ getDistances rest

shiftPoint : Point -> Bounds -> Point
shiftPoint point bounds = { x = point.x - bounds.left
                          , y = point.y - bounds.bottom
                          }

addMarkerToGrid : Bounds -> Point -> Grid -> Grid
addMarkerToGrid bounds marker grid = 
  let
    shiftedPoint = shiftPoint marker bounds
    updatedRow =
      case Array.get shiftedPoint.y grid of
         Just row -> Array.set shiftedPoint.x { coordinates = 
                                                 { x = marker.x
                                                 , y = marker.y
                                                 }
                                               , status = Marker
                                               } row
         Nothing -> Array.empty
  in
    Array.set shiftedPoint.y updatedRow grid

hasDuplicates : List comparable -> Bool
hasDuplicates list =
  case list of
     [] -> False
     x :: xs -> List.any ((==) x) xs || hasDuplicates xs

getStatus : List Point -> Point -> CellStatus
getStatus markers point =
  let
    distances = getDistances markers
  in
    if List.any ((==) point) markers then
      Marker
    else if List.any (\marker -> List.any ((==) (getDistance point marker)) distances) markers then
      Invalid
    else if hasDuplicates (List.map (getDistance point) markers) then
      Invalid
    else
      Empty

getGrid : Model -> Grid
getGrid model =
  let
    bounds = getBounds model
    makeCell y x = 
      let
        point = { x = x
                , y = y
                }
      in
        { coordinates = point
        , status = getStatus model.markers point
        }
    makeRow y = Array.map (makeCell y) (fromList (List.range bounds.left bounds.right))
  in
    Array.map makeRow (fromList (List.range bounds.bottom bounds.top))

solve : Model -> Model
solve model = 
  if List.length model.markers == model.size then model
  else
    let
      grid = getGrid model
      flatGrid = List.concat (toList (Array.map toList grid))
      options = List.map .coordinates (List.filter (\cell -> cell.status == Empty) flatGrid)
      findBest newPoint running =
        if List.length running.markers == model.size then
          running
        else
          let candidate = solve {model | markers = newPoint :: model.markers}
          in
            if List.length candidate.markers > List.length running.markers then
              candidate
            else
              running
    in
      List.foldl findBest model options

-- UPDATE
type Msg = Solve
         | Reset
         | AddMarker Point
         | ChangeSize String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Solve -> solve model
    Reset -> init
    AddMarker point -> { model | markers = point :: model.markers}
    ChangeSize newSizeStr -> case toInt newSizeStr of
       Nothing -> model
       Just newSize -> if newSize > 2 then
                         makeModel newSize 
                       else
                         model

-- VIEW
getHtmlFromCell : Cell -> Html Msg
getHtmlFromCell cell =
  case cell.status of
     Empty -> td [onClick (AddMarker cell.coordinates)] []
     Invalid -> td [class "invalid"] []
     Marker -> td [] [text "O"]

getRowFromGridRow : Array Cell -> Html Msg
getRowFromGridRow row = tr [] (toList (Array.map getHtmlFromCell row))

view : Model -> Html Msg
view model =
  let grid = getGrid model
  in
    div []
      [ div [] [ label [] [text "Grid Size: "]
               , input [ value (fromInt model.size)
                       , onInput ChangeSize
                       ] []
               ]
      , table [] (toList (Array.map getRowFromGridRow grid))
      , div [] [ text ("Markers remaining: " ++ fromInt (model.size - List.length model.markers))]
      , button [onClick Solve] [text "Solve!"]
      , button [onClick Reset] [text "Reset"]
      ]