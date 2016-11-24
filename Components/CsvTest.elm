module Components.CsvTest exposing (main)

import Html exposing (..)
import Html.App exposing (..)
import Html.Attributes exposing (..)

import CommonViews
import Components.Csv as Csv

main : Program Never
main =
    let
        spec = Csv.spec {}
    in
        program { spec | view = CommonViews.debugView spec.view }
