-- TODO: I've been thinking about move validation all wrong. If we
-- don't remove pieces that have been jumped when we construct our tree
-- of jump opportunities, it's theoretically possible to jump in an
-- infinite circle. We still need to construct a tree of possible
-- moves, but each of the nodes of the tree must contain the whole
-- board state


module Main-old exposing (..)


import Array exposing (Array)
import Browser
import Html exposing (Html, table, tr, td, text)
import Html.Events exposing (onClick)
import List.Extra exposing (groupsOf)

squaresPerRow : Int
squaresPerRow = 8

playingSquaresPerRow : Int
playingSquaresPerRow = squaresPerRow // 2

numberOfSquares : Int
numberOfSquares = squaresPerRow^2



-- MAIN


-- main =
--   Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type PieceColor
  = Red
  | White

type PieceClass
  = Man
  | King

type Piece
  = Piece PieceColor PieceClass

type PlayingSquare
  = Vacant
  | Occupied Piece

type Square
  = NonPlayingSquare
  | PlayingSquare PlayingSquare

type alias Board = Array Square

type alias SquareIndex = Int

type alias JumpSequence = List SquareIndex

type alias SimpleMove = (SquareIndex, SquareIndex)

type Move
  = SimpleMove SimpleMove
  | JumpMove JumpSequence

type JumpOpportunities
  = NoJumpOpportunities SquareIndex
  | OneJumpOpportunity SquareIndex JumpOpportunities
  | TwoJumpOpportunities SquareIndex JumpOpportunities JumpOpportunities
  | ThreeJumpOpportunities SquareIndex JumpOpportunities JumpOpportunities JumpOpportunities
  | FourJumpOpportunities SquareIndex JumpOpportunities JumpOpportunities JumpOpportunities JumpOpportunities

type Direction
  = Northwest
  | Northeast
  | Southwest
  | Southeast

type alias Model =
  { board : Board
  , moves : List Move
  , turn : PieceColor
  }

createRow : Square -> Square -> List Square
createRow firstOfPair secondOfPair =
  let
    pairs = List.repeat playingSquaresPerRow [firstOfPair, secondOfPair]
  in
    List.concat pairs

init : Model
init =
  let
    redStartingRows = List.concat
      [ createRow NonPlayingSquare (PlayingSquare (Occupied (Piece Red Man)))
      , createRow (PlayingSquare (Occupied (Piece Red Man))) NonPlayingSquare
      , createRow NonPlayingSquare (PlayingSquare (Occupied (Piece Red Man)))
      ]
    vacantRows = List.concat
      [ createRow (PlayingSquare Vacant) NonPlayingSquare
      , createRow NonPlayingSquare (PlayingSquare Vacant)
      ]
    whiteStartingRows = List.concat
      [ createRow (PlayingSquare (Occupied (Piece White Man))) NonPlayingSquare
      , createRow NonPlayingSquare (PlayingSquare (Occupied (Piece White Man)))
      , createRow (PlayingSquare (Occupied (Piece White Man))) NonPlayingSquare
      ]
    squareList = List.concat
      [ redStartingRows
      , vacantRows
      , whiteStartingRows
      ]
  in
    { board = Array.fromList squareList
    , moves = []
    , turn = White
    }

getSquare : Board -> SquareIndex -> Maybe Square
getSquare board squareIndex =
  Array.get squareIndex board

getAdjacentSquareIndex : SquareIndex -> Direction -> SquareIndex
getAdjacentSquareIndex originIndex direction =
  case direction of
    Northwest -> originIndex - squaresPerRow - 1
    Northeast -> originIndex - squaresPerRow + 1
    Southwest -> originIndex + squaresPerRow - 1
    Southeast -> originIndex + squaresPerRow + 1

getJumpSquareIndex : SquareIndex -> Direction -> SquareIndex
getJumpSquareIndex originIndex direction =
  case direction of
     Northwest -> originIndex - (squaresPerRow * 2) - 2
     Northeast -> originIndex - (squaresPerRow * 2) + 2
     Southwest -> originIndex + (squaresPerRow * 2) - 2
     Southeast -> originIndex + (squaresPerRow * 2) + 2

isVacant : Board -> SquareIndex -> Bool
isVacant board squareIndex =
  let
    square = getSquare board squareIndex
  in
    case square of
      Just (PlayingSquare Vacant) -> True
      _ -> False

getAllowedSimpleMoves : Board -> SquareIndex -> List Move
getAllowedSimpleMoves board originIndex =
  let
    originSquare = getSquare board originIndex
    getAdacentFromOrigin = getAdjacentSquareIndex originIndex
    adjacentSquareIndices =
      case originSquare of
        Just (PlayingSquare (Occupied (Piece Red Man))) ->
          [ getAdacentFromOrigin Southwest
          , getAdacentFromOrigin Southeast
          ]
        Just (PlayingSquare (Occupied (Piece White Man))) ->
          [ getAdacentFromOrigin Northwest
          , getAdacentFromOrigin Northeast
          ]
        Just (PlayingSquare (Occupied (Piece _ King))) ->
          [ getAdacentFromOrigin Northwest
          , getAdacentFromOrigin Northeast
          , getAdacentFromOrigin Southwest
          , getAdacentFromOrigin Southeast
          ]
        _ ->
          []
    allowedDestinationIndices = List.filter (isVacant board) adjacentSquareIndices
    createMove destinationIndex = SimpleMove originIndex destinationIndex
  in
    List.map createMove allowedDestinationIndices

getJumpOpportunities : Board -> Piece -> SquareIndex -> JumpOpportunities
getJumpOpportunities board jumpingPiece originIndex =
  let
    nwIndex = getJumpSquareIndex originIndex Northwest
    neIndex = getJumpSquareIndex originIndex Northeast
    swIndex = getJumpSquareIndex originIndex Southwest
    seIndex = getJumpSquareIndex originIndex Southeast
    nwVacant = isVacant board nwIndex
    neVacant = isVacant board neIndex
    swVacant = isVacant board swIndex
    seVacant = isVacant board seIndex
    -- TODO: Might need to add a previousOriginIndex to stop jumping
    -- when the king's row is reached
    iterate = getJumpOpportunities board jumpingPiece
  in
    case (jumpingPiece, (nwVacant, neVacant), (swVacant, seVacant)) of
      ((Piece Red Man), _, (False, False)) -> NoJumpOpportunities originIndex
      ((Piece Red Man), _, (False, True)) -> OneJumpOpportunity originIndex (iterate seIndex)
      ((Piece Red Man), _, (True, False)) -> OneJumpOpportunity originIndex (iterate swIndex)
      ((Piece Red Man), _, (True, True)) -> TwoJumpOpportunities originIndex (iterate swIndex) (iterate seIndex)
      ((Piece White Man), (False, False), _) -> NoJumpOpportunities originIndex
      ((Piece White Man), (False, True), _) -> OneJumpOpportunity originIndex (iterate neIndex)
      ((Piece White Man), (True, False), _) -> OneJumpOpportunity originIndex (iterate nwIndex)
      ((Piece White Man), (True, True), _) -> TwoJumpOpportunities originIndex (iterate nwIndex) (iterate neIndex)
      ((Piece _ King), (False, False), (False, False)) -> NoJumpOpportunities originIndex
      ((Piece _ King), (False, False), (False, True)) -> OneJumpOpportunity originIndex (iterate seIndex)
      ((Piece _ King), (False, False), (True, False)) -> OneJumpOpportunity originIndex (iterate seIndex)  
      ((Piece _ King), (False, False), (True, True)) -> TwoJumpOpportunities originIndex (iterate swIndex) (iterate seIndex)
      ((Piece _ King), (False, True), (False, False)) -> OneJumpOpportunity originIndex (iterate neIndex)
      ((Piece _ King), (False, True), (False, True)) -> TwoJumpOpportunities originIndex (iterate neIndex) (iterate seIndex)
      ((Piece _ King), (False, True), (True, False)) -> TwoJumpOpportunities originIndex (iterate neIndex) (iterate swIndex)
      ((Piece _ King), (False, True), (True, True)) -> ThreeJumpOpportunities originIndex (iterate neIndex) (iterate swIndex) (iterate seIndex)
      ((Piece _ King), (True, False), (False, False)) -> OneJumpOpportunity originIndex (iterate nwIndex)
      ((Piece _ King), (True, False), (False, True)) -> TwoJumpOpportunities originIndex (iterate nwIndex) (iterate seIndex)
      ((Piece _ King), (True, False), (True, False)) -> TwoJumpOpportunities originIndex (iterate nwIndex) (iterate swIndex)
      ((Piece _ King), (True, False), (True, True)) -> ThreeJumpOpportunities originIndex (iterate nwIndex) (iterate swIndex) (iterate seIndex)
      ((Piece _ King), (True, True), (False, False)) -> TwoJumpOpportunities originIndex (iterate nwIndex) (iterate neIndex)
      ((Piece _ King), (True, True), (False, True)) -> ThreeJumpOpportunities originIndex (iterate nwIndex) (iterate neIndex) (iterate seIndex)
      ((Piece _ King), (True, True), (True, False)) -> ThreeJumpOpportunities originIndex (iterate nwIndex) (iterate neIndex) (iterate swIndex)
      ((Piece _ King), (True, True), (True, True)) -> FourJumpOpportunities originIndex (iterate nwIndex) (iterate neIndex) (iterate swIndex) (iterate seIndex)

-- jumpOpportunitiesToListOfMoves : JumpOpportunities -> List Move

-- getAllowedJumpMoves : Board -> SquareIndex -> List Move
-- getAllowedJumpMoves board originIndex
  -- Get the origin square
  -- Create a list of adjacent-index/adjacent-square/jump-index/jump-square tuples
  -- Filter the list down to those that reflect jumping an opponent piece
  -- Map the list of tuples to a list of one-jump moves
  -- Fold the list of one-jump moves into a list of one- and multi-jump moves
    -- Use recursion
    -- Stop jumping at the kings row
  -- Filter the list

-- getAllowedMoves : Board -> SquareIndex -> List Move
-- getAllowedMoves board originIndex =
--   -- Get all the allowed simple moves for the piece
--   -- Get all the allowed jump moves for the piece
--   -- If there are any jump moves
--     -- Return only the jump moves
--   -- Else
--     -- Return the simple moves

-- validateMove : Board -> Move -> Boolean
-- validateMove board move =
--   -- Get all the valid moves for piece
--   -- Is the move specified among them?

-- getJumpedIndices : Board -> Move -> List Integer
-- getJumpedIndices board move =
--   -- If not JumpMove
--     -- Return empty list
--   -- Else
--     -- THEN WHAT?


-- applyMoveToBoard : Board -> Move -> Maybe Board
--   -- Is move valid?
--   -- If not, return Nothing
--   -- Get indices of jumped pieces
--   -- Map the board squares
--     -- If index matches the final index of the move
--       -- Return the originating piece
--     -- Else if it matches the originating index of the move
--       -- Return an empty square
--     -- Else if it matches one of the jumped indices
--       -- Return an empty square



-- UPDATE


-- type alias Msg = Move


-- update : Msg -> Model -> Model
-- update msg model =
--   let
--     -- TODO: Write and implement a functions that a) validate the move,
--     -- and b) update the board based on the move
--     moves = model.moves ++ [msg]
--     turn =
--       case model.turn of
--         White -> Red
--         Red -> White
--   in
--     { board = model.board
--     , moves = moves
--     , turn = turn
--     }


-- VIEW

-- renderSquare : Square -> Html td
-- renderSquare square =
--   let
--     cellText =
--       case square of
--         NonPlayingSquare -> "NP"
--         Playing Vacant -> "V"
--         Playing (Occupied (Piece Red Man)) -> "RM"
--         Playing (Occupied (Piece Red King)) -> "RK"
--         Playing (Occupied (Piece White Man)) -> "WM"
--         Playing (Occupied (Piece White King)) -> "WK"
--   in
--     td []
--       [ text cellText ]

-- renderRow : List Square -> Html tr
-- renderRow squares =
--   let
--     renderedSquares = List.map renderSquare squares
--   in
--     tr [] renderedSquares

-- boardToRows : Board -> List (List Square)
-- boardToRows squares =
--   groupsOf squaresPerRow squares

-- view : Model -> Html Msg
-- view model =
--   let
--     rows = boardToRows model.board
--     renderedRows = List.map renderRow rows
--   in
--     table [] renderedRows


-- view : Model -> Html Msg
-- view model =
--   div []
--     [ button [ onClick Decrement ] [ text "-" ]
--     , div [] [ text (String.fromInt model) ]
--     , button [ onClick Increment ] [ text "+" ]
--     ]
