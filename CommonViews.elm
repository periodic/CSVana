module CommonViews exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http

import Base

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

debugView : (Base.Instance data msg -> Html msg) -> Base.Instance data msg -> Html msg
debugView childView instance =
    div []
        [ childView instance
        , div [ class "Debug" ]
            [ text <| toString <| Base.get instance ]
        ]

withDebug : Base.Program (Base.Instance data msg) msg -> Base.Program (Base.Instance data msg) msg
withDebug program =
    { program | view = debugView program.view }
