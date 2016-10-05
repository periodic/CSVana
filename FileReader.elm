port module FileReader exposing (..)

import Json.Decode exposing (Decoder, at, object3, string, int, value, keyValuePairs, maybe, (:=), map)
import Html exposing (Attribute)
import Html.Events exposing (on)

type alias FileRef = Json.Decode.Value

type alias FileInfo =
    { name : String
    , size : Int
    , blob : FileRef
    }

port readFile : FileInfo -> Cmd msg

port fileChunk : (String -> msg) -> Sub msg

onFileInput : (List FileInfo -> msg) -> Attribute msg
onFileInput wrapper =
    on "change" (map wrapper parseFiles)

parseFiles : Decoder (List FileInfo)
parseFiles =
    at [ "target", "files" ] <|
        map (List.filterMap snd) (keyValuePairs <| maybe fileInfoParser)

fileInfoParser : Decoder FileInfo
fileInfoParser =
    object3 FileInfo
        ("name" := string)
        ("size" := int)
        value
