module Util exposing (..)

transpose : List (List a) -> List (List a)
transpose data =
    let
        heads = List.map List.head data |> catMaybes
        tails = List.map List.tail data |> catMaybes
    in 
        if List.isEmpty heads
            then []
            else heads :: transpose tails

catMaybes : List (Maybe a) -> List a
catMaybes xs =
    case (List.head xs, List.tail xs) of
        (Just (Just x), Just xs) ->
            x :: catMaybes xs
        (Just Nothing, Just xs) ->
            catMaybes xs
        (_, Nothing) ->
            []
        (Nothing, _) ->
            []

find : (a -> Bool) -> List a -> Maybe a
find pred xs =
    case List.head xs of
        Just x ->
            if pred x
            then Just x
            else List.tail xs `Maybe.andThen` find pred
        Nothing ->
            List.tail xs `Maybe.andThen` find pred
