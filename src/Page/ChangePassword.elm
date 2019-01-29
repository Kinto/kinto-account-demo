module Page.ChangePassword exposing (Model, Msg, init, update, view)

import Css exposing (color, rgb)
import Data.Session exposing (Session)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Http exposing (Error(..))
import Request.Accounts exposing (changePassword, errorToString)


type alias Model =
    { login : String
    , currentPassword : String
    , newPassword : String
    , confirmNewPassword : String
    , error : Maybe String
    , passwordChanged : Bool
    }


type Msg
    = SetServer String
    | SetEmail String
    | SetCurrentPassword String
    | SetNewPassword String
    | SetConfirmNewPassword String
    | ChangePassword
    | PasswordChanged (Result Error ())


init : String -> Session -> ( Model, Session, Cmd Msg )
init currentPassword session =
    ( { login = session.store.login
      , currentPassword = currentPassword
      , newPassword = ""
      , confirmNewPassword = ""
      , error = Nothing
      , passwordChanged = False
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

        SetCurrentPassword value ->
            ( { model | currentPassword = value }, session, Cmd.none )

        SetNewPassword value ->
            ( { model | newPassword = value }, session, Cmd.none )

        SetConfirmNewPassword value ->
            ( { model | confirmNewPassword = value }, session, Cmd.none )

        ChangePassword ->
            if model.newPassword /= model.confirmNewPassword then
                ( { model
                    | error = Just "Password don't match"
                    , newPassword = ""
                    , confirmNewPassword = ""
                  }
                , session
                , Cmd.none
                )

            else
                ( model
                , session
                , changePassword PasswordChanged
                    session
                    model.login
                    model.currentPassword
                    model.newPassword
                )

        PasswordChanged (Ok ()) ->
            ( { model
                | passwordChanged = True
                , login = ""
                , currentPassword = ""
                , newPassword = ""
                , confirmNewPassword = ""
                , error = Nothing
              }
            , session
            , Cmd.none
            )

        PasswordChanged (Err err) ->
            ( { model
                | error = Just <| errorToString err
                , currentPassword = ""
                , newPassword = ""
                , confirmNewPassword = ""
              }
            , session
            , Cmd.none
            )


view : Session -> Model -> ( String, List (Html Msg) )
view session model =
    let
        style =
            if model.newPassword == model.confirmNewPassword then
                -- darkgreen
                [ color (rgb 12 120 12) ]

            else
                -- darkred
                [ color (rgb 120 12 12) ]
    in
    ( "Change password"
    , [ h1 [] [ text "Change your account password" ]
      , Html.form [ onSubmit ChangePassword ]
            [ displayError model.error
            , if model.passwordChanged then
                div [ css [ color (rgb 12 120 12) ] ] [ text "Account password updated" ]

              else
                div [] []
            , p []
                [ label []
                    [ text "Server: "
                    , input
                        [ type_ "text"
                        , onInput SetServer
                        , value session.store.server
                        ]
                        []
                    ]
                ]
            , p []
                [ label []
                    [ text "E-mail: "
                    , input
                        [ type_ "text"
                        , onInput SetEmail
                        , value model.login
                        ]
                        []
                    ]
                ]
            , p []
                [ label []
                    [ text "Current password: "
                    , input
                        [ type_ "password"
                        , onInput SetCurrentPassword
                        , value model.currentPassword
                        ]
                        []
                    ]
                ]
            , p []
                [ label []
                    [ text "New password: "
                    , input
                        [ type_ "password"
                        , onInput SetNewPassword
                        , value model.newPassword
                        ]
                        []
                    ]
                ]
            , p []
                [ label [ css style ]
                    [ text "Confirm new password: "
                    , input
                        [ type_ "password"
                        , onInput SetConfirmNewPassword
                        , value model.confirmNewPassword
                        ]
                        []
                    ]
                ]
            , button [ type_ "submit" ] [ text "Change password" ]
            ]
      ]
    )


displayError : Maybe String -> Html Msg
displayError error =
    case error of
        Nothing ->
            div [] []

        Just err ->
            div [ css [ color (rgb 120 12 12) ] ] [ text err ]
