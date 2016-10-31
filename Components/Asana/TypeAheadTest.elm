import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Html.App exposing (program)

import Components.Asana.TypeAhead as TypeAhead
import Components.Asana.Model as Asana


type alias Model =
    { typeAhead : TypeAhead.TypeAhead Asana.Project
    , fragment : String
    }

init : (Model, Cmd Msg)
init =
    ({ typeAhead = TypeAhead.init "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJhdXRob3JpemF0aW9uIjoxOTI5Njk3NjgzOTEzMzEsInNjb3BlIjoiZGVmYXVsdCIsImlhdCI6MTQ3Nzg4NTk1OCwiZXhwIjoxNDc3ODg5NTU4fQ.OjoamBpJDRgyiU--rO-EvV1MpKyR_nxTLHNPcxmeVh0" (Asana.Id 16202385315778) TypeAhead.getProjectOptions
    , fragment = ""
    }, Cmd.none)

type Msg
    = TypeAheadMsg (TypeAhead.Msg Asana.Project)
    | NewInput String


view : Model -> Html Msg
view {typeAhead, fragment} =
    div []
        [ input [ onInput NewInput, value fragment ] []
        , Html.App.map TypeAheadMsg (TypeAhead.render typeAhead)
        ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        TypeAheadMsg msg' ->
            let
                (typeAhead', cmd) = TypeAhead.update msg' model.typeAhead
            in
               ({model | typeAhead = typeAhead'}, Cmd.map TypeAheadMsg cmd)
        NewInput fragment ->
            let
                (typeAhead', cmd) = TypeAhead.update (TypeAhead.TypeAheadInput fragment) model.typeAhead
            in
                ({ model | fragment = fragment, typeAhead = typeAhead' }, Cmd.map TypeAheadMsg cmd)

main : Program Never
main =
    program
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
