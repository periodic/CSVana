module Components.FormSectionTest exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

import Base exposing (Instance)
import Asana.Model as Asana
import CommonViews
import Components.FormSection as FormSection

completeComponent : String -> (Instance () (), Cmd ())
completeComponent str =
    Base.create
        { init = (str, Cmd.none)
        , update = always <| always (str, Cmd.none)
        , view = always <| text str
        , subscriptions = always Sub.none
        , get = always ()
        }

incompleteComponent : Maybe String -> (Instance (Maybe String) String, Cmd String)
incompleteComponent str =
    Base.create
        { init = (Maybe.withDefault "" str, Cmd.none)
        , update = \msg -> always (msg, Cmd.none)
        , view = \str -> input
                [ type_ "text"
                , onInput identity
                , value str
                ] []
        , subscriptions = always Sub.none
        , get = \str ->
            if String.isEmpty str
                then Nothing
                else Just str
        }

main : Program Never (FormSection.Instance String String ()) (FormSection.Msg String ())
main =
    Html.program
    <| CommonViews.withDebug
    <| Base.asRoot
    <| FormSection.create
        { incompleteChild = incompleteComponent
        , completeChild = completeComponent
        , value = Nothing
        }
