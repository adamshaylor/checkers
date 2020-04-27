module Main exposing (..)


import Browser
import Html exposing (Html, table, tr, td, text)
import Html.Events exposing (onClick)
import List.Extra exposing (groupsOf)

size : Int
size = 8

playingSquaresPerRow: Int
playingSquaresPerRow = size // 2

numberOfSquares : Int
numberOfSquares = size^2



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type PieceColor
  = Red
  | White

type PieceClass
  = Man
  | King

type Piece
  = Piece PieceColor PieceClass

type Square
  = NonPlaying
  | Vacant
  | Occupied Piece

type alias Move = List Int

type alias Model =
  { board : List Square
  , moves : List Move
  }

makeRow : Square -> Square -> List Square
makeRow square1 square2 =
  let
    pairs = List.repeat playingSquaresPerRow [square1, square2]
  in
    List.concat pairs


init : Model
init =
  let
    redsStartingRows = List.concat
      [ makeRow NonPlaying (Occupied (Piece Red Man))
      , makeRow (Occupied (Piece Red Man)) NonPlaying
      , makeRow NonPlaying (Occupied (Piece Red Man))
      ]
    vacantRows = List.concat
      [ makeRow Vacant NonPlaying
      , makeRow NonPlaying Vacant
      ]
    whitesStartingRows = List.concat
      [ makeRow (Occupied (Piece White Man)) NonPlaying
      , makeRow NonPlaying (Occupied (Piece White Man))
      , makeRow (Occupied (Piece White Man)) NonPlaying
      ]
  in
    { board = List.concat
      [ redsStartingRows
      , vacantRows
      , whitesStartingRows
      ]
    , moves = []
    }


-- UPDATE


type alias Msg = Move


update : Msg -> Model -> Model
update msg model =
  let
    -- TODO: Write and implement a functions that a) validate the move,
    -- and b) update the board based on the move
    moves = model.moves ++ [msg]
  in
    { board = model.board
    , moves = moves
    }


-- VIEW

renderSquare : Square -> Html td
renderSquare square =
  let
    cellText =
      case square of
        NonPlaying -> "NP"
        Vacant -> "V"
        Occupied (Piece Red Man) -> "RM"
        Occupied (Piece Red King) -> "RK"
        Occupied (Piece White Man) -> "WM"
        Occupied (Piece White King) -> "WK"
  in
    td []
      [ text cellText ]

renderRow : List Square -> Html tr
renderRow squares =
  let
    renderedSquares = List.map renderSquare squares
  in
    tr [] renderedSquares

boardToRows : List Square -> List (List Square)
boardToRows squares =
  groupsOf size squares

view : Model -> Html Msg
view model =
  let
    rows = boardToRows model.board
    renderedRows = List.map renderRow rows
  in
    table [] renderedRows


-- view : Model -> Html Msg
-- view model =
--   div []
--     [ button [ onClick Decrement ] [ text "-" ]
--     , div [] [ text (String.fromInt model) ]
--     , button [ onClick Increment ] [ text "+" ]
--     ]
