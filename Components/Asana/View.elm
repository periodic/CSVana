module Components.Asana.View exposing (..)

import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http

import Components.OAuth as OAuth
import Components.Asana exposing (Msg(..), Model, FormResource)
import Components.Asana.Model exposing (..)
import Components.Asana.ApiResource as ApiResource
import Components.Asana.ApiResource exposing (ApiResource)
import Components.Asana.Form as Form

view : Model -> Html Msg
view model =
    case OAuth.getState model.oauth of
        OAuth.Success _ ->
            viewOAuthSuccess model
        OAuth.Failure msg ->
            viewOAuthError msg
        _ ->
            viewOAuthLoading

viewOAuthSuccess : Model -> Html Msg
viewOAuthSuccess model =
    div [ class "OAuth--success" ]
        [ viewForm model.form ]

viewForm : FormResource -> Html Msg
viewForm =
    let
        unloaded = div [] []
    in
        Html.App.map FormMsg << ApiResource.view unloaded loadingIndicator errorView Form.view

loadingIndicator : Html msg
loadingIndicator =
    div [ class "LoadingIndicator" ]
        [ text "Loading..." ]

errorView : Http.Error -> Html msg
errorView error =
    div [ class "ApiError" ]
        [ text <| toString error ]

viewOAuthLoading =
    div [ class "OAuth--loading" ]
        [ loadingIndicator ]

viewOAuthError msg =
    div [ class "OAuth--error" ]
        [ text msg ]
