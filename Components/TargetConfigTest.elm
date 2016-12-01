module Components.TargetConfigTest exposing (main)

import Html exposing (..)
import Html.Events as Events
import Html.App exposing (program)
import String

import Base
import Components.TargetConfig as TargetConfig
import Components.Clearable as Clearable
import CommonViews

numberSelector : (Base.Instance (Maybe Int) Int, Cmd Int)
numberSelector =
    Base.create
        { init = (Nothing, Cmd.none)
        , update = \msg _ -> (Just msg, Cmd.none)
        , subscriptions = always Sub.none
        , view = always
             <| div []
                (List.map (\val -> a [ Events.onClick val ] [ text <| toString val ]) [1,2,3])
        , get = identity
        }


main =
    program
    <| CommonViews.withDebug
    <| Base.asRoot
    <| TargetConfig.create
        { defaultMap = String.toInt >> Result.toMaybe
        , dataView = \value -> 
            Clearable.create
                { value = value
                , emptyView = numberSelector
                , fullView = Base.staticComponent << text << toString
                }
        , records = [ "", "1", "2", "foo" ]
        }


