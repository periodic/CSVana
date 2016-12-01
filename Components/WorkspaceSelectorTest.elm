module Components.WorkspaceSelectorTest exposing (main)

import Html.App exposing (..)

import Asana.Model as Asana
import Base
import Components.WorkspaceSelector as WorkspaceSelector
import CommonViews

workspaces : List Asana.WorkspaceResource
workspaces =
    [ { id = "1", name = "First" }
    , { id = "2", name = "Second" }
    , { id = "foo", name = "Foo-th" }
    ]

main : Program Never
main =
    program
    <| CommonViews.withDebug
    <| Base.asRoot
    <| WorkspaceSelector.create { workspaces = workspaces }
