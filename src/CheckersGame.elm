module CheckersGame exposing (..)

import Array exposing (Array)

playingSquaresPerRow : Int
playingSquaresPerRow = 4

numberOfPlayingSquares : Int
numberOfPlayingSquares = playingSquaresPerRow^2

type Color
  = Red
  | White

type Rank
  = Man
  | King

type Piece
  = Piece Color Rank

type PlayingSquare
  = Vacant
  | Occupied Piece

type alias BoardState = Array PlayingSquare
type alias InitialBoardState = BoardState

type alias PlayingSquareIndex = Int

type Direction
  = NW
  | NE
  | SE
  | SW

type RelativeDirection
  = Forward
  | Backward

type DirectionalMove
  = SimpleMove PlayingSquareIndex Direction
  | JumpMove PlayingSquareIndex Direction

type IndexedMove
  = IndexedMove PlayingSquareIndex PlayingSquareIndex

type Turn
  = Turn Color (List IndexedMove)

type alias Turns = List Turn

type alias WinningColor = Color

type Game
  = Game InitialBoardState Turns
  | ResignedGame InitialBoardState Turns WinningColor

squareIndexToRowIndex : PlayingSquareIndex -> Int
squareIndexToRowIndex squareIndex =
  playingSquaresPerRow // squareIndex

getMoveOriginIndex : DirectionalMove -> Int
getMoveOriginIndex directionalMove =
  case directionalMove of
    SimpleMove originIndex _ -> originIndex
    JumpMove originIndex _ -> originIndex

getAdjacentIndex: Int -> Direction -> Int
getAdjacentIndex originIndex direction =
  let
    originRowIndex = squareIndexToRowIndex originIndex
    originRowIsEven = (modBy 2 originRowIndex) == 0
  in
    case (direction, originRowIsEven) of
      (NW, True) -> originIndex - playingSquaresPerRow
      (NW, False) -> originIndex - playingSquaresPerRow - 1
      (NE, True) -> originIndex - playingSquaresPerRow + 1
      (NE, False) -> originIndex - playingSquaresPerRow
      (SE, True) -> originIndex + playingSquaresPerRow + 1
      (SE, False) -> originIndex + playingSquaresPerRow
      (SW, True) -> originIndex + playingSquaresPerRow
      (SW, False) -> originIndex + playingSquaresPerRow - 1

absoluteToRelativeDirection: Direction -> Color -> RelativeDirection
absoluteToRelativeDirection absoluteDirection color =
  case (absoluteDirection, color) of
    (NW, Red) -> Backward
    (NE, Red) -> Backward
    (SE, Red) -> Forward
    (SW, Red) -> Forward
    (NW, White) -> Forward
    (NE, White) -> Forward
    (SE, White) -> Backward
    (SW, White) -> Backward

getMoveDestinationIndex : DirectionalMove -> Int
getMoveDestinationIndex directionalMove =
  case directionalMove of
    SimpleMove originIndex direction ->
      getAdjacentIndex originIndex direction
    JumpMove originIndex direction ->
      getAdjacentIndex (getAdjacentIndex originIndex direction) direction

getMoveOriginSquare : DirectionalMove -> BoardState -> Maybe PlayingSquare
getMoveOriginSquare direcitonalMove boardState =
  Array.get (getMoveOriginIndex direcitonalMove) boardState

getMoveAbsoluteDirection : DirectionalMove -> Direction
getMoveAbsoluteDirection directionalMove =
  case directionalMove of
    SimpleMove _ direction -> direction
    JumpMove _ direction -> direction

getMoveRelativeDirection : DirectionalMove -> BoardState -> Maybe RelativeDirection
getMoveRelativeDirection directionalMove boardState =
  let
    originSquare = getMoveOriginSquare directionalMove boardState
    absoluteDirection = getMoveAbsoluteDirection directionalMove
  in
    case originSquare of
      Just (Occupied (Piece color _)) -> Just (absoluteToRelativeDirection absoluteDirection color)
      _ -> Nothing

getMoveDestinationSquare : DirectionalMove -> BoardState -> Maybe PlayingSquare
getMoveDestinationSquare directionalMove boardState =
  let
    destinationIndex = getMoveDestinationIndex directionalMove
  in
    Array.get destinationIndex boardState

getMoveJumpSquare : DirectionalMove -> BoardState -> Maybe PlayingSquare
getMoveJumpSquare directionalMove boardState =
  case directionalMove of
    JumpMove originIndex direction ->
      let
        jumpSquareIndex = getAdjacentIndex originIndex direction
      in
        Array.get jumpSquareIndex boardState
    SimpleMove _ _ ->
      Nothing

isValidDirectionalMove : DirectionalMove -> BoardState -> Bool
isValidDirectionalMove directionalMove boardState =
  let
    originSquare = getMoveOriginSquare directionalMove boardState
    relativeDirection = getMoveRelativeDirection directionalMove boardState
    destinationSquare = getMoveDestinationSquare directionalMove boardState
    jumpSquare = getMoveJumpSquare directionalMove boardState
    isKingOrForwardMovingMan =
      case (originSquare, relativeDirection) of
        (Just (Occupied (Piece _ King)), _) -> True
        (Just (Occupied (Piece _ Man)), Just Forward) -> True
        (Just (Occupied (Piece _ Man)), Just Backward) -> False
        (Nothing, _) -> False
        (Just Vacant, _) -> False
        (_, Nothing) -> False
  in
    if isKingOrForwardMovingMan then
      case directionalMove of
        SimpleMove _ _ ->
          case (originSquare, destinationSquare) of
            (Just (Occupied _), Just Vacant) -> True
            (_, _) -> False
        JumpMove _ _ ->
          case (originSquare, jumpSquare, destinationSquare) of
            (Just (Occupied (Piece Red _)), Just (Occupied (Piece White _)), Just Vacant) -> True
            (Just (Occupied (Piece White _)), Just (Occupied (Piece Red _)), Just Vacant) -> True
            (_, _, _) -> False
      else
        False

directionalToIndexedMove : DirectionalMove -> IndexedMove
directionalToIndexedMove directionalMove =
  case directionalMove of
    SimpleMove originIndex _ ->
      IndexedMove originIndex (getMoveDestinationIndex directionalMove)
    JumpMove originIndex _ ->
      IndexedMove originIndex (getMoveDestinationIndex directionalMove)

validMoves : PlayingSquareIndex -> BoardState -> List IndexedMove
validMoves originIndex boardState =
  let
    allDirectionalMoves =
      [ SimpleMove originIndex NW
      , SimpleMove originIndex NE
      , SimpleMove originIndex SE
      , SimpleMove originIndex SW
      , JumpMove originIndex NW
      , JumpMove originIndex NE
      , JumpMove originIndex SE
      , JumpMove originIndex SW 
      ]
    validDirectionalMoves = List.filter
      (\directionalMove -> isValidDirectionalMove directionalMove boardState)
      allDirectionalMoves
  in
    List.map directionalToIndexedMove validDirectionalMoves
