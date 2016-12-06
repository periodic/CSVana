module Components.FieldRow exposing (Props, Msg, Data, Instance, create)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import List
import Set exposing (Set)
import String

import Base
import Components.TargetSelector as TargetSelector
import Asana.Api as Api
import Asana.Model as Asana

type alias Props =
    { customFields : List Asana.CustomFieldInfo
    , records : Set String
    , header : String
    , apiContext : Api.Context
    }

type alias Msg = TargetSelector.Msg
type alias Data = TargetSelector.Data
type alias Instance = Base.Instance Data Msg

create : Props -> (Instance, Cmd Msg)
create props =
    Base.create
        { init = TargetSelector.create
            { customFields = props.customFields
            , records = props.records
            , apiContext = props.apiContext
            }
        , update = Base.update
        , subscriptions = Base.subscriptions
        , view = view props
        , get = Base.get
        }

--------------------------------------------------------------------------------
-- Private

type alias Model =
    TargetSelector.Instance

view : Props -> TargetSelector.Instance -> Html Msg
view props inst =
    div [ class "FieldRow Grid" ]
        [ div [ class "FieldRow-csv Cell -5of12" ]
            [ div [ class "FieldRow-csvHeader" ]
                [ text props.header ]
            , div [ class "FieldRow-csvValues" ]
                [ Set.toList props.records |> sampleString |> text ]
            ]
        , div [ class "FieldRow-selector Cell -5of12" ]
            [ Base.view inst ]
        ]

sampleString : List String -> String
sampleString samples =
    let
        shownSamples =  List.filter (String.isEmpty >> not) samples |> List.take 3 |> String.join ", "
    in
        if List.length samples > 3
            then shownSamples ++ "…"
            else shownSamples

