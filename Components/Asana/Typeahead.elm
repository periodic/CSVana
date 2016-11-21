module Components.Asana.Typeahead exposing (..)

import Components.Asana.Model exposing (..)
import Components.Asana.Api exposing (ApiResult)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (Decoder, list)

type ResourceStatus a
    = Unloaded
    | Loading
    | Loaded a

type alias Model a =
    { options : ResourceStatus (List a)
    , fragment : String
    , selected : Maybe a
    -- TODO: move read only values to props.
    , token : String
    , workspaceId : Id
    , fetcher : Id -> String -> String -> Cmd (ApiResult (List a))
    }

init : String -> Id -> (Id -> String -> String -> Cmd (ApiResult (List a))) -> (Model a, Cmd (Msg a))
init token workspaceId fetcher =
    ({ options = Unloaded
    , fragment = ""
    , selected = Nothing
    , token = token
    , workspaceId = workspaceId
    , fetcher = fetcher
    }, Cmd.none)

type Msg a
    = NewTypeaheadOptions (ApiResult (List a))
    | TypeaheadInput String
    | TypeaheadSelection a

update : Msg { a | name : String } -> Model { a | name : String } -> (Model { a | name : String }, Cmd (Msg { a | name : String }))
update msg model =
    case (Debug.log "Typeahead update: " msg) of
        NewTypeaheadOptions (Ok items) ->
            ({model | options = Loaded items}, Cmd.none)
        NewTypeaheadOptions (Err items) ->
            ({model | options = Unloaded}, Cmd.none)
        TypeaheadInput fragment ->
            if fragment == ""
               then ({ model | options = Unloaded, fragment = fragment, selected = Nothing }, Cmd.none)
               else ({ model | options = Loading, fragment = fragment, selected = Nothing },
                    Cmd.map NewTypeaheadOptions <| model.fetcher model.workspaceId fragment model.token)
        TypeaheadSelection val ->
            ({ model | selected = Just val, fragment = val.name, options = Unloaded }, Cmd.none)

view : Model { a | name : String} -> Html (Msg { a | name : String })
view model =
    div []
        [ input [ onInput TypeaheadInput, value model.fragment ] []
        , renderOptions model
        ]

renderOptions : Model { a | name : String } -> Html (Msg { a | name : String })
renderOptions {options, selected} =
    div [ class "Typeahead" ]
        (case options of
            Loaded items ->
                List.map (renderOption selected) items
            Loading ->
                [ renderLoadingOption ]
            Unloaded ->
                [])

renderOption : Maybe { a | name : String } -> { a | name : String } -> Html (Msg { a | name : String })
renderOption selected resource =
    case selected of
        Just selectedResource ->
            if selectedResource == resource
                then renderSelected resource
                else renderUnselected resource
        Nothing ->
            renderUnselected resource

renderUnselected : { a | name : String } -> Html (Msg { a | name : String })
renderUnselected resource = 
    div [ class "Typeahead_option"
        , onClick (TypeaheadSelection resource)
        ]
        [ text resource.name ]

renderSelected : { a | name : String } -> Html (Msg { a | name : String })
renderSelected resource = 
    div [ class "Typeahead_option Typeahead_option--selected"
        , onClick (TypeaheadSelection resource)
        ]
        [ text resource.name ]

renderLoadingOption : Html (Msg { a | name : String  })
renderLoadingOption =
    div [ class "Typeahead_option__loading"
        ]
        [ text "Loading..." ]

getSelection : Model a -> Maybe a
getSelection = .selected
