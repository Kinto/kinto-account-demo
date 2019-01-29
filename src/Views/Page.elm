module Views.Page exposing (ActivePage(..), Config, frame)

import Browser exposing (Document)
import Css exposing (..)
import Data.Session exposing (Session)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css, href)
import Route
import Views.Theme exposing (Element, defaultCss)


type ActivePage
    = Home
    | Validate
    | ResetPassword
    | ChangePassword
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
            , text " | "
            , linkIf ResetPassword Route.ResetPassword "Reset my password"
            , text " | "
            , linkIf ChangePassword (Route.ChangePassword "") "Change my password"
            ]
        ]
