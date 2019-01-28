module Views.Page exposing (ActivePage(..), Config, frame)

import Browser exposing (Document)
import Css exposing (..)
import Data.Session exposing (Session)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css, href, src)
import Route
import Views.Theme exposing (Element, defaultCss)


type ActivePage
    = Home
    | Validate
    | Other


type alias Config =
    { session : Session
    , activePage : ActivePage
    }


frame : Config -> ( String, List (Html msg) ) -> Document msg
frame config ( title, content ) =
    { title = title ++ " | kinto-account"
    , body =
        [ div []
            [ defaultCss
            , viewHeader config
            , div [ css [ padding2 (Css.em 1) zero ] ] content
            ]
            |> toUnstyled
        ]
    }


githubIconStyle : Element msg
githubIconStyle =
    styled a
        [ position absolute
        , top (px 15)
        , right (px 15)
        , border3 (px 1) solid (rgba 255 255 255 0.3)
        , padding (px 10)
        , borderRadius (px 4)
        , color (hex "999")
        , textDecoration none
        ]


heading1 : Element msg
heading1 =
    styled h1
        [ textAlign center
        , margin2 (Css.em 1) zero
        , color (hex "000")
        , fontSize (px 60)
        , lineHeight (px 1)
        ]


viewHeader : Config -> Html msg
viewHeader { activePage } =
    let
        linkIf page route caption =
            if page == activePage then
                strong [] [ text caption ]

            else
                a [ Route.href route ] [ text caption ]
    in
    div [ class "header" ]
        [ heading1 [] [ text "kinto-accounts" ]
        , div [ css [ textAlign center ] ]
            [ linkIf Home Route.Home "Create an account"
            , text " | "
            , linkIf Validate (Route.Validate "") "Validate your account"
            ]
        ]
