module Components.TargetSelectorTest exposing (main)

import Html.App
import Set

import Base
import Asana.Model as Asana
import CommonViews
import Components.TargetSelector as TargetSelector


customFields : List Asana.CustomFieldInfo
customFields =
    [ Asana.CustomTextFieldInfo "216726495168048" "String"
    , Asana.CustomNumberFieldInfo "216726495168054" "Drew's Number" 2
    , Asana.CustomEnumFieldInfo "216726495168050" "Drew's Enum"
        [ { id = "216726495168051", name = "Option 1" }
        , { id = "216726495168052", name = "Option 2" }
        ]
    ]

main : Program Never
main =
    Html.App.program
    <| CommonViews.withDebug
    <| Base.asRoot
    <| TargetSelector.create
        { customFields = customFields
        , records = Set.fromList [ "1", "True", "Done", "Foo", "1" ]
        , token = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJhdXRob3JpemF0aW9uIjoyMTU4Njg1MjQ5MzQxNTMsInNjb3BlIjoiZGVmYXVsdCIsImlhdCI6MTQ4MDk4ODA5MCwiZXhwIjoxNDgwOTkxNjkwfQ.EmdQIqGh59YqKzsL83hwQVUEzKqvowtuSKCfo8uEFjY"
        , workspaceId = "15793206719"
        }
