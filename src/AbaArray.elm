module AbaArray exposing (..)
import Array exposing (Array)

type AbaArray a b
  = EmptyAbaArray
  | NonEmptyAbaArray a (Array (b, a))

empty : AbaArray a b
empty = EmptyAbaArray

isEmpty : AbaArray a b -> Bool
isEmpty abaArray =
  case abaArray of
    EmptyAbaArray -> True
    _ -> False

getA : Int -> AbaArray a b -> Maybe a
getA i abaArray =
  case (i, abaArray) of
    (_, EmptyAbaArray) ->
      Nothing
    (0, NonEmptyAbaArray a _) ->
      Just a
    (_, NonEmptyAbaArray _ baArray) ->
      Array.get (i - 1) baArray
        |> Maybe.andThen (\(_, a) -> Just a)
