module Route exposing (Route(..), fromUrl, href, pushUrl, toString)

import Browser.Navigation as Nav
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = Home
    | Validate String
    | ResetPassword
    | ChangePassword String


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map (Validate "") (Parser.s "validate")
        , Parser.map Validate (Parser.s "validate" </> Parser.string)
        , Parser.map ResetPassword (Parser.s "reset-password")
        , Parser.map ChangePassword (Parser.s "reset-password" </> Parser.string)
        , Parser.map (ChangePassword "") (Parser.s "change-password")
        , Parser.map ChangePassword (Parser.s "reset-password" </> Parser.string)
        , Parser.map ChangePassword (Parser.s "change-password" </> Parser.string)
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser


href : Route -> Attribute msg
href route =
    Attr.href (toString route)


pushUrl : Nav.Key -> Route -> Cmd msg
pushUrl key route =
    Nav.pushUrl key (toString route)


toString : Route -> String
toString route =
    let
        pieces =
            case route of
                Home ->
                    []

                Validate activationKey ->
                    [ "validate", activationKey ]

                ResetPassword ->
                    [ "reset-password" ]

                ChangePassword currentPassword ->
                    [ "change-password", currentPassword ]
    in
    "#/" ++ String.join "/" pieces
