module Components.TargetSelectorTest exposing (main)

import Html.App

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
        }
