module Components.FieldOptions exposing (Props, Msg, Data, Instance, create)

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)
import Set exposing (Set)

import Base
import Util
import Asana.Model as Asana
import Asana.Target as Target exposing (Target)
import Components.FieldRow as FieldRow

type alias Props =
    { customFields : List Asana.CustomFieldInfo
    , numFields : Int
    , headers : List (String)
    , records : List (List String)
    }

type Msg
    = ChildMsg Int (FieldRow.Msg)

type alias Data = List (Maybe Target)
type alias Instance = Base.Instance Data Msg

create : Props -> (Instance, Cmd Msg)
create props =
    Base.create
        { init = init props
        , update = update props
        , view = view props
        , subscriptions = subscriptions
        , get = Array.map (Base.get) >> Array.toList
        }

--------------------------------------------------------------------------------
-- Private

type alias Model =
    Array (FieldRow.Instance)

init : Props -> (Model, Cmd Msg)
init { customFields, records, headers } =
    let
        columns = Util.transpose (Debug.log "Row-major: " records) |> Debug.log "Column-major: "
        fields = List.map2 (,) headers columns
            |> List.map (\(header, column) ->
                FieldRow.create
                    { customFields = customFields
                    , records = Set.fromList column
                    , header = header
                    })
        instances = List.map fst fields |> Array.fromList
        cmds = List.indexedMap (\index inst -> snd inst |> Cmd.map (ChildMsg index)) fields |> Cmd.batch
    in
        (instances, cmds)

update : Props -> Msg -> Model -> (Model, Cmd Msg)
update props msg model =
    case msg of
        ChildMsg index msg' ->
            case Array.get index model of
                Just inst ->
                    let
                        (inst', cmd) = Base.updateWith (ChildMsg index) msg' inst
                    in
                        (Array.set index inst' model, cmd)
                Nothing ->
                    (model, Cmd.none)


view : Props -> Model -> Html Msg
view props model =
    div [ class "FieldOptions" ]
        (Array.indexedMap viewSelector model |> Array.toList)

subscriptions : Model -> Sub Msg
subscriptions =
    Array.toList >> List.indexedMap (ChildMsg >> Base.subscriptionsWith) >> Sub.batch

viewSelector : Int -> FieldRow.Instance -> Html Msg
viewSelector index selector =
    div [ class "FieldOptions-field" ]
        [ Base.viewWith (ChildMsg index) selector ]

