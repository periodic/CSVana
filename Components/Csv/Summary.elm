module Components.Csv.Summary exposing (Props, Msg, Data, Instance, create)

import Html exposing (Html, text, div, span)
import Html.Attributes exposing (class)

import Base

type alias CsvData = (List String, List (List String))

type alias Props =
    { csvData : CsvData
    }

type alias Msg =
    ()

type alias Data =
    ()

type alias Instance = Base.Instance Data Msg

create : Props -> (Instance, Cmd Msg)
create { csvData } =
    Base.staticComponent <| view csvData

--------------------------------------------------------------------------------
-- Private

view : CsvData -> Html Msg
view csvData =
    Tuple.first csvData
        |> List.map (\header -> span [] [ text header ])
        |> List.intersperse (text ",")
        |> div [ class "CsvSummary" ]
