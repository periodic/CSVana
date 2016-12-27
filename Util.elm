module Util exposing (..)

mapFirst : (a -> b) -> (a, c) -> (b, c)
mapFirst f (a, c) =
    (f a, c)

mapSecond : (a -> b) -> (c, a) -> (c, b)
mapSecond f (c, a) =
    (c, f a)

mapPair : (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapPair f g (a, b) =
    (f a, g b)

mapCmd : (msg1 -> msg2) -> (a, Cmd msg1) -> (a, Cmd msg2)
mapCmd f =
    mapSecond (Cmd.map f)

isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Just _ ->
            True
        Nothing ->
            False

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
            else List.tail xs |> Maybe.andThen (find pred)
        Nothing ->
            List.tail xs |> Maybe.andThen (find pred)

