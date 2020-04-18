module Main exposing (..)


import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type Color
  = Red
  | Black

type Class
  = Man
  | King

type alias Piece =
  { color : Color
  , class : Class
  }

type Square =
  = Occupied Piece
  | Empty

type alias Move =
  { fromIndex : Int
  , toIndex : Int
  }

type alias Model =
  { pieces : List Square
  , moves : List Moves
  }

init : Model
init =
  let
    piecesPerColor = 12
    gapBetweenColors = 8
  in
    List.append (List.repeat piecesPerColor (Square Occupied Piece Red Man)) <|
    List.append (List.repeat gapBetweenColors (Square Empty)) <|
    List.repeat piecesPerColor (Square Occupied Piece Black Man)



-- UPDATE


type Msg
  = Increment
  | Decrement


update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model) ]
    , button [ onClick Increment ] [ text "+" ]
    ]