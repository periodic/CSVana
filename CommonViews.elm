module CommonViews exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Http
import Svg exposing (svg, polygon, path)
import Svg.Attributes exposing (viewBox, points, d)

import Base

unloadedView : Html msg
unloadedView =
    div [] []

loadingIndicator : Html msg
loadingIndicator =
    div [ class "LoadingIndicator" ]
        [ text "Loading..." ]

errorView : Http.Error -> Html msg
errorView error =
    div [ class "ApiError" ]
        [ text <| toString error ]

debugView : (Base.Instance data msg -> Html msg) -> Base.Instance data msg -> Html msg
debugView childView instance =
    div []
        [ childView instance
        , div [ class "Debug" ]
            [ text <| toString <| Base.get instance ]
        ]

withDebug : Base.Program (Base.Instance data msg) msg -> Base.Program (Base.Instance data msg) msg
withDebug program =
    { program | view = debugView program.view }

popup : String -> msg -> Html msg -> Html msg
popup title closeMsg inner =
    div [ class "Popup" ]
        [ div [ class "Popup-positioner"]
            [ div [ class "Popup-background"] []
            ]
        , div [ class "Popup-positioner"]
            [ div [ class "Popup-container"]
                [ div [ class "Popup-dialog" ]
                    [ closeButton closeMsg
                    , div [ class "Popup-title" ]
                        [ text title ]
                    , div [ class "Popup-contents" ]
                        [ inner ]
                    ]
                ]
            ]
        ]

configButton : msg -> Html msg
configButton msg =
    a [ class "openButton", Events.onClick msg ] [ gearIcon ]

closeButton : msg -> Html msg
closeButton msg =
    a [ class "closeButton", Events.onClick msg ] [ closeIcon ]

iconStyle : Attribute msg
iconStyle =
    style [("width", "16px"), ("height", "16px")] 

closeIcon : Html msg
closeIcon =
    Svg.svg [ Svg.Attributes.class "icon closeIcon", viewBox "0 0 32 32", iconStyle ]
        [ Svg.polygon
            [ points "24.485,27.314 27.314,24.485 18.828,16 27.314,7.515 24.485,4.686 16,13.172 7.515,4.686 4.686,7.515 13.172,16 4.686,24.485 7.515,27.314 16,18.828 " ]
            []
        ]

gearIcon : Html msg
gearIcon =
    Svg.svg [ Svg.Attributes.class "icon gearIcon", viewBox "0 0 512 512", iconStyle ]
        [ Svg.path [ d "M512,288v-64l-69.156-11.531c-4.813-20.781-13-40.188-23.969-57.781l40.781-57.063l-45.25-45.25l-57.094,40.75  c-17.594-10.938-37-19.156-57.781-24L288,0h-64l-11.531,69.125c-20.75,4.844-40.188,13.063-57.781,24l-57.094-40.75l-45.25,45.25  l40.781,57.063c-10.969,17.563-19.156,37-23.969,57.781L0,224v64l69.156,11.531c4.813,20.781,13,40.188,23.969,57.781  l-40.781,57.094l45.25,45.25l57.125-40.781c17.563,10.969,37,19.156,57.75,23.969L224,512h64l11.531-69.156  c20.75-4.813,40.188-13,57.781-23.969l57.094,40.781l45.25-45.25l-40.781-57.094c10.969-17.594,19.156-37.031,23.969-57.781L512,288  z M256,384c-70.688,0-128-57.313-128-128s57.313-128,128-128s128,57.313,128,128S326.688,384,256,384z" ] []
        ]

