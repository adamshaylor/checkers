module StateTransitionHistory exposing
  ( create
  , append
  , removeLastNStates
  , now
  , getNStatesAgo
  , getTransitionToNStatesAgo
  , getTransitionFromNStatesAgo
  )

import Array exposing (Array)

type alias StateTransitionHistory s t =
  { currentState: s
  , past: Array (t, s)
  }

create : s -> StateTransitionHistory s t
create initialState = StateTransitionHistory initialState Array.empty

append : (StateTransitionHistory s t -> t -> Result e s) -> t -> StateTransitionHistory s t -> Result e (StateTransitionHistory s t)
append transitionFunction transition history =
  let
    updatedPast = Array.push (transition, history.currentState) history.past
    transitionResult = transitionFunction history transition
  in
    case transitionResult of
      Ok newState -> Ok (StateTransitionHistory newState updatedPast)
      Err error -> Err error

removeLastNStates : Int -> StateTransitionHistory s t -> Maybe (StateTransitionHistory s t)
removeLastNStates n history =
  let
    truncatedPast = Array.slice 0 (n * -1) history.past
    maybeLastRemainingState = getNStatesAgo (n + 1) history
  in
    case compare n 0 of
      LT -> Nothing
      EQ -> Nothing
      GT -> maybeLastRemainingState
        |> Maybe.andThen (\nMinusOneStatesAgo -> Just (StateTransitionHistory nMinusOneStatesAgo truncatedPast))

now : StateTransitionHistory s t -> s
now history = history.currentState

getNStatesAgo : Int -> StateTransitionHistory s t -> Maybe s
getNStatesAgo n history =
  let
    pastLength = Array.length history.past
    pastIndexForN = pastLength - n
  in
    case compare n 0 of
      LT -> Nothing
      EQ -> Just history.currentState
      GT -> Array.get pastIndexForN history.past
        |> Maybe.andThen (\(_, s) -> Just s)

getTransitionToNStatesAgo : Int -> StateTransitionHistory s t -> Maybe t
getTransitionToNStatesAgo n history =
  let
    pastLength = Array.length history.past
    indexOfTransitionToNStatesAgo = pastLength - n - 1
  in
    case compare n 0 of
      LT -> Nothing
      _ -> Array.get indexOfTransitionToNStatesAgo history.past
        |> Maybe.andThen (\(t, _) -> Just t)

getTransitionFromNStatesAgo : Int -> StateTransitionHistory s t -> Maybe t
getTransitionFromNStatesAgo n history =
  let
    pastLength = Array.length history.past
    indexOfTransitionFromNStatesAgo = pastLength - n
  in
    case compare n 0 of
      LT -> Nothing
      _ -> Array.get indexOfTransitionFromNStatesAgo history.past
        |> Maybe.andThen (\(t, _) -> Just t)
