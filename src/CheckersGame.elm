module CheckersGame exposing (..)

import StaticArray exposing (StaticArray)
import StaticArray.Index exposing (FourPlus, Four)
import StateTransitionHistory exposing (create, append)


type Color
  = Red
  | White

type alias Player = Color

type PieceType
  = Man
  | King

type alias Piece =
  { color: Color
  , pieceType: PieceType
  }

type SquareState
  = Occupied Piece
  | Vacant

type alias PlayingSquareIndex
  = FourPlus
  ( FourPlus
  ( FourPlus
  ( FourPlus
  ( FourPlus
  ( FourPlus
  ( FourPlus
  ( Four
  )))))))

type alias BoardState = StaticArray PlayingSquareIndex SquareState

type MoveType
  = SimpleMove
  | JumpMove

type RelativeDirection
  = ForwardLeft
  | ForwardRight
  | BackLeft
  | BackRight

type alias Move =
  { player: Player
  , originIndex: PlayingSquareIndex
  , destinationIndex: PlayingSquareIndex
  , direction: RelativeDirection
  , moveType: MoveType
  }

type alias GameState =
  { boardState: BoardState
  , whitePiecesCaptured: Int
  , redPiecesCaptured: Int
  , turnPlayer: Maybe Player
  , allowedNextMoves: List Move
  , winner: Maybe Player
  }

-- type alias GameState =
--   { boardState: BoardState
--   , whitePiecesCaptured: Int
--   , redPiecesCaptured: Int
--   , turnPlayer: Maybe Player
--   , allowedNextMoves: List Move
--   , winner: Maybe Player
--   , history: List ActionHistory
--   }

-- type alias ActionSideEffect =
--   { allowed: Bool
--   , captureIndex: Maybe PlayingSquareIndex
--   , crowningIndex: Maybe PlayingSquareIndex
--   , gameState: GameState
--   }

-- type ActionHistory
--   = StartedGame Player ActionSideEffect
--   | MadeMove Move ActionSideEffect
--   | Resigned Player ActionSideEffect

-- type alias Game = List ActionHistory

-- startGame : Player -> BoardState -> GameState
-- startGame
