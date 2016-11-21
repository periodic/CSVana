module Components.Asana.View exposing (..)

import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http

import Components.OAuth as OAuth
import Components.Asana exposing (Msg(..), Model)
import Components.Asana.Model exposing (..)
import Components.Asana.ApiResource as ApiResource
import Components.Asana.ApiResource exposing (ApiResource)
import Components.Asana.WorkspaceSelector as WorkspaceSelector

view : Model -> Html Msg
view model =
    case OAuth.getState model.oauth of
        OAuth.Success _ ->
            viewOAuthSuccess model
        OAuth.Failure msg ->
            viewOAuthError msg
        _ ->
            viewOAuthLoading

viewOAuthSuccess model =
    div [ class "OAuth--success" ]
        [ projectForm model.currentUser ]

projectForm : ApiResource User -> Html Msg
projectForm currentUserResource =
    apiResource
        (.workspaces
            >> Maybe.withDefault []
            >> (\ws -> { selected = Nothing, workspaces = ws })
            >> WorkspaceSelector.view
            >> Html.App.map WorkspaceSelectorMsg)
        currentUserResource


apiResource : (a -> Html Msg) -> ApiResource a -> Html Msg
apiResource subView =
    let
        unloaded = div [] []
    in
        ApiResource.view unloaded loadingIndicator errorView subView

loadingIndicator : Html Msg
loadingIndicator =
    div [ class "LoadingIndicator" ]
        [ text "Loading..." ]

errorView : Http.Error -> Html Msg
errorView error =
    div [ class "ApiError" ]
        [ text <| toString error ]


viewOAuthLoading =
    div [ class "OAuth--loading" ]
        [ loadingIndicator ]

viewOAuthError msg =
    div [ class "OAuth--error" ]
        [ text msg ]
