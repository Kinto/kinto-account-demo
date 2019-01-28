module Page.Home exposing (Model, Msg(..), init, update, view)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Css exposing (..)
import Data.Session exposing (Session)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Http exposing (Error(..))
import Markdown
import Request.Accounts exposing (createAccount, errorToString)
import Route exposing (Route(..))
import Task


type alias Model =
    { login : String
    , password : String
    , confirm_password : String
    , error : Maybe String
    }


type Msg
    = SetServer String
    | SetEmail String
    | SetPassword String
    | SetConfirmPassword String
    | CreateAccount
    | AccountCreated (Result Error ())


init : Session -> ( Model, Session, Cmd Msg )
init session =
    ( { login = ""
      , password = ""
      , confirm_password = ""
      , error = Nothing
      }
    , session
    , Cmd.none
    )


update : Session -> Msg -> Model -> ( Model, Session, Cmd Msg )
update ({ store } as session) msg model =
    case msg of
        SetServer value ->
            let
                newStore =
                    { store | server = value }
            in
            ( model, { session | store = newStore }, Cmd.none )

        SetEmail value ->
            ( { model | login = value }, session, Cmd.none )

        SetPassword value ->
            ( { model | password = value }, session, Cmd.none )

        SetConfirmPassword value ->
            ( { model | confirm_password = value }, session, Cmd.none )

        CreateAccount ->
            if model.password /= model.confirm_password then
                ( { model | error = Just "Password don't match", password = "", confirm_password = "" }, session, Cmd.none )

            else
                let
                    newStore =
                        { store | login = model.login }
                in
                ( { model | login = "", password = "", confirm_password = "", error = Nothing }
                , { session | store = newStore }
                , createAccount AccountCreated session model.login model.password
                )

        AccountCreated (Ok ()) ->
            ( model
            , session
            , Nav.pushUrl session.navKey (Route.toString (Route.Validate ""))
            )

        AccountCreated (Err err) ->
            ( { model | error = Just <| errorToString err }, session, Cmd.none )


displayError : Maybe String -> Html Msg
displayError error =
    case error of
        Nothing ->
            div [] []

        Just err ->
            div [ css [ color (rgb 120 12 12) ] ] [ text err ]


view : Session -> Model -> ( String, List (Html Msg) )
view session model =
    let
        style =
            if model.password == model.confirm_password then
                -- darkgreen
                [ color (rgb 12 120 12) ]

            else
                -- darkred
                [ color (rgb 120 12 12) ]
    in
    ( "Home"
    , [ Html.form [ onSubmit CreateAccount ]
            [ h1 [] [ text "Create an account" ]
            , displayError model.error
            , p []
                [ label []
                    [ text "Server: "
                    , input [ type_ "text", onInput SetServer, value session.store.server ] []
                    ]
                ]
            , p []
                [ label []
                    [ text "E-mail: "
                    , input [ type_ "text", onInput SetEmail, value model.login ] []
                    ]
                ]
            , p []
                [ label []
                    [ text "Password: "
                    , input [ type_ "password", onInput SetPassword, value model.password ] []
                    ]
                ]
            , p []
                [ label [ css style ]
                    [ text "Confirm password: "
                    , input
                        [ type_ "password"
                        , onInput SetConfirmPassword
                        , value model.confirm_password
                        ]
                        []
                    ]
                ]
            , button [ type_ "submit" ] [ text "Create account" ]
            ]
      ]
    )
