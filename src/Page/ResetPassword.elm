module Page.ResetPassword exposing (Model, Msg, init, update, view)

import Css exposing (color, rgb)
import Data.Session exposing (Session)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Http exposing (Error(..))
import Request.Accounts exposing (errorToString, resetPassword)


type alias Model =
    { login : String
    , error : Maybe String
    , reset : Bool
    }


type Msg
    = SetServer String
    | SetEmail String
    | ResetPassword
    | AccountReset (Result Error ())


init : Session -> ( Model, Session, Cmd Msg )
init session =
    ( { login = session.store.login
      , error = Nothing
      , reset = False
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

        ResetPassword ->
            ( model, session, resetPassword AccountReset session model.login )

        AccountReset (Ok ()) ->
            ( { model | reset = True, login = "", error = Nothing }
            , session
            , Cmd.none
            )

        AccountReset (Err err) ->
            ( { model | error = Just <| errorToString err }, session, Cmd.none )


view : Session -> Model -> ( String, List (Html Msg) )
view session model =
    ( "Reset Password"
    , [ h1 [] [ text "Reset your account password" ]
      , Html.form [ onSubmit ResetPassword ]
            [ displayError model.error
            , if model.reset then
                div [ css [ color (rgb 12 120 12) ] ] [ text "Mail sent" ]

              else
                div [] []
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
            , button [ type_ "submit" ] [ text "Reset account password" ]
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
