module Components.CsvTest exposing (main)

import Html.App exposing (..)

import Base
import CommonViews
import Components.Csv as Csv

main : Program Never
main =
    program <| CommonViews.withDebug <| Base.asRoot <| Csv.create {}
