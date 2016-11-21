module Components.Asana.CommonViews exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

import Http

unloadedView : Html msg
unloadedView =
    div [] []

loadingIndicator : Html msg
loadingIndicator =
    div [ class "LoadingIndicator" ]
        [ text "Loading..." ]

errorView : Http.Error -> Html msg
errorView error =
    div [ class "ApiError" ]
        [ text <| toString error ]

