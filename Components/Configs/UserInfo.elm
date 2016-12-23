module Components.Configs.UserInfo exposing (Props, Instance, create)

import Asana.Model as Asana
import Base
import Html exposing (..)
import Html.Attributes exposing (..)

type alias Props =
    { user : Asana.User
    }

type alias Instance msg = Base.Instance () msg


create : Props -> (Instance msg, Cmd msg)
create { user } =
    Base.staticComponent (view user)

--------------------------------------------------------------------------------
-- Private

view : Asana.User -> Html msg
view { name, photo } =
    div [ class "UserInfo" ]
        [ case photo |> Maybe.andThen .image_21x21 of
            Just photo ->
                img [ class "UserInfo-photo avatar", src photo ] []
            Nothing ->
                text ""
        , span [ class "UserInfo-name" ]
            [ text name ]
        ]
