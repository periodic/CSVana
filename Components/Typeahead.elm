module Components.Typeahead exposing (Props, Msg, Instance, create)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onFocus, onBlur, onMouseOver, onMouseOut)

import Asana.Api as Api
import Asana.Model as Asana exposing (Named)
import Base

type alias Props a =
    { fetcher : String -> Cmd (Api.ApiResult (List a))
    }

type Msg a
    = NewOptions String (Api.ApiResult (List a))
    | Input String
    | Selection a
    | InputFocus Bool
    | OptionsHovered Bool

type alias Data a = Maybe a

type alias Instance a = Base.Instance (Data a) (Msg a)

create : Props (Named a) -> (Instance (Named a), Cmd (Msg (Named a)))
create props =
    Base.create 
        { init = init props
        , update = update props
        , subscriptions = always Sub.none
        , view = view props
        , get = get
        }

--------------------------------------------------------------------------------
-- Private

type ResourceStatus a
    = Unloaded
    | Loading
    | Loaded a

type alias Model a =
    { options : ResourceStatus (List a)
    , fragment : String
    , selected : Maybe a
    , inputFocused : Bool
    , optionsHovered : Bool
    }

init : Props a -> (Model a, Cmd (Msg a))
init _ =
    let
        model =
            { options = Unloaded
            , fragment = ""
            , selected = Nothing
            , inputFocused = False
            , optionsHovered = False
            }
    in
        (model, Cmd.none)

update : Props (Named a) -> Msg (Named a) -> Model (Named a) -> (Model (Named a), Cmd (Msg (Named a)))
update { fetcher } msg model =
    case msg of
        NewOptions fragment (Ok items) ->
            if fragment == model.fragment && isActive model
                then ({model | options = Loaded items}, Cmd.none)
                else (model, Cmd.none)
        NewOptions _ (Err items) ->
            (model, Cmd.none)
        Input fragment ->
            if fragment == ""
               then ({ model | options = Unloaded, fragment = fragment, selected = Nothing, optionsHovered = False }, Cmd.none)
               else ({ model | options = Loading, fragment = fragment, selected = Nothing },
                    Cmd.map (NewOptions fragment) <| fetcher fragment)
        Selection val ->
            ({ model | selected = Just val, fragment = val.name, options = Unloaded }, Cmd.none)
        InputFocus val ->
            ({ model | inputFocused = val }, Cmd.none)
        OptionsHovered val ->
            ({ model | optionsHovered = val }, Cmd.none)

view : Props (Named a) -> Model (Named a) -> Html (Msg (Named a))
view _ model =
    let
        inputElem = input
            [ class "Typeahead-input"
            , type' "text"
            , onInput Input
            , onFocus <| InputFocus True
            , onBlur <| InputFocus False
            , value model.fragment
            ] []
        elems =
            if isActive model
            then [ inputElem, renderOptions model ]
            else [ inputElem ]
    in
        div [ class "Typeahead" ] elems

get : Model a -> Maybe a
get = .selected

isActive : Model a -> Bool
isActive model =
    case model.options of
        Unloaded ->
            False
        _ ->
            model.inputFocused || model.optionsHovered

renderOptions : Model (Named a) -> Html (Msg (Named a))
renderOptions {options, selected} =
        case options of
            Loaded items ->
                div [ class "Typeahead-options"
                    , onMouseOver (OptionsHovered True)
                    , onMouseOut (OptionsHovered False)
                    ]
                    (if List.length items > 0
                        then List.map (renderOption selected) items
                        else [ renderNoResultsOption ])
            Loading ->
                div [ class "Typeahead-options" ]
                    [ renderLoadingOption ]
            Unloaded ->
                div [ class "Typeahead-options--empty" ]
                    []

renderOption : Maybe (Named a) -> (Named a) -> Html (Msg (Named a))
renderOption selected resource =
    case selected of
        Just selectedResource ->
            if selectedResource == resource
                then renderSelected resource
                else renderUnselected resource
        Nothing ->
            renderUnselected resource

renderUnselected : (Named a) -> Html (Msg (Named a))
renderUnselected resource = 
    div [ class "Typeahead-option"
        , onClick (Selection resource)
        ]
        [ text resource.name ]

renderSelected : (Named a) -> Html (Msg (Named a))
renderSelected resource = 
    div [ class "Typeahead-option Typeahead-option--selected"
        , onClick (Selection resource)
        ]
        [ text resource.name ]

renderLoadingOption : Html (Msg a)
renderLoadingOption =
    div [ class "Typeahead-option Typeahead-option--loading"
        ]
        [ text "Loading..." ]

renderNoResultsOption : Html (Msg a)
renderNoResultsOption =
    div [ class "Typeahead-option Typeahead-option--noResults"
        ]
        [ text "No Results" ]
