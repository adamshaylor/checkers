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

type alias Model = Array Maybe Piece

init : Model
init =
  let
    piecesPerColor = 12
    gapBetweenColors = 8
  in
    Array.append (Array.repeat piecesPerColor (Just (Piece Red Man))) <|
    Array.append (Array.repeat gapBetweenColors Nothing) <|
    Array.repeat piecesPerColor (Just (Piece Black Man))



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