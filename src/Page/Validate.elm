module Page.Validate exposing (Model, Msg, init, update, view)

import Css exposing (color, fontSize, margin2, rgb, zero)
import Data.Session as Session exposing (Session)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Http exposing (Error(..))
import Ports
import Request.Accounts exposing (errorToString, validateKey)
import Route
import Views.Theme exposing (Element)


type alias Model =
    { login : String
    , key : String
    , error : Maybe String
    , validated : Bool
    }


type Msg
    = SetServer String
    | SetEmail String
    | SetActivationKey String
    | Validate
    | AccountValidated (Result Error ())


init : String -> Session -> ( Model, Session, Cmd Msg )
init activationKey session =
    ( { login = session.store.login
      , key = activationKey
      , error = Nothing
      , validated = False
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

        SetActivationKey value ->
            ( { model | key = value }, session, Cmd.none )

        Validate ->
            ( model, session, validateKey AccountValidated session model.login model.key )

        AccountValidated (Ok ()) ->
            ( { model | validated = True, login = "", key = "" }
            , session
            , Cmd.none
            )

        AccountValidated (Err err) ->
            ( { model | error = Just <| errorToString err }, session, Cmd.none )


view : Session -> Model -> ( String, List (Html Msg) )
view _ model =
    ( "Validate"
    , [ h1 [] [ text "Validate your account" ]
      , Html.form [ onSubmit Validate ]
            [ displayError model.error
            , if model.validated then
                div [ css [ color (rgb 12 120 12) ] ] [ text "Account validated" ]

              else
                div [] []
            , p []
                [ label []
                    [ text "E-mail: "
                    , input [ type_ "text", onInput SetEmail, value model.login ] []
                    ]
                ]
            , p []
                [ label []
                    [ text "Activation Key: "
                    , input [ type_ "text", onInput SetActivationKey, value model.key ] []
                    ]
                ]
            , button [ type_ "submit" ] [ text "Validate account" ]
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
