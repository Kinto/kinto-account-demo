module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Data.Session as Session exposing (Session)
import Html.Styled as Html exposing (..)
import Page.ChangePassword as ChangePassword
import Page.Home as Home
import Page.ResetPassword as ResetPassword
import Page.Validate as Validate
import Ports
import Route exposing (Route)
import Url exposing (Url)
import Views.Page as Page


type alias Flags =
    { clientUrl : String
    , rawStore : String
    }


type Page
    = Blank
    | HomePage Home.Model
    | ValidatePage Validate.Model
    | ResetPasswordPage ResetPassword.Model
    | ChangePasswordPage ChangePassword.Model
    | NotFound


type alias Model =
    { page : Page
    , session : Session
    }


type Msg
    = HomeMsg Home.Msg
    | ValidateMsg Validate.Msg
    | ResetPasswordMsg ResetPassword.Msg
    | ChangePasswordMsg ChangePassword.Msg
    | StoreChanged String
    | UrlChanged Url
    | UrlRequested Browser.UrlRequest


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    let
        toPage page subInit subMsg =
            let
                ( subModel, newSession, subCmds ) =
                    subInit model.session
            in
            ( { model | session = newSession, page = page subModel }
            , Cmd.map subMsg subCmds
            )
    in
    case maybeRoute of
        Nothing ->
            ( { model | page = NotFound }
            , Cmd.none
            )

        Just Route.Home ->
            toPage HomePage Home.init HomeMsg

        Just (Route.Validate activationKey) ->
            toPage ValidatePage (Validate.init activationKey) ValidateMsg

        Just Route.ResetPassword ->
            toPage ResetPasswordPage ResetPassword.init ResetPasswordMsg

        Just (Route.ChangePassword currentPassword) ->
            toPage ChangePasswordPage (ChangePassword.init currentPassword) ChangePasswordMsg


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        session =
            { clientUrl = flags.clientUrl
            , navKey = navKey
            , store = Session.deserializeStore flags.rawStore
            }
    in
    setRoute (Route.fromUrl url)
        { page = Blank
        , session = session
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ page, session } as model) =
    let
        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newSession, newCmd ) =
                    subUpdate session subMsg subModel

                storeCmd =
                    if session.store /= newSession.store then
                        newSession.store |> Session.serializeStore |> Ports.saveStore

                    else
                        Cmd.none
            in
            ( { model | session = newSession, page = toModel newModel }
            , Cmd.map toMsg (Cmd.batch [ newCmd, storeCmd ])
            )
    in
    case ( msg, page ) of
        ( HomeMsg homeMsg, HomePage homeModel ) ->
            toPage HomePage HomeMsg Home.update homeMsg homeModel

        ( ValidateMsg validateMsg, ValidatePage validateModel ) ->
            toPage ValidatePage ValidateMsg Validate.update validateMsg validateModel

        ( ResetPasswordMsg resetPasswordMsg, ResetPasswordPage resetPasswordModel ) ->
            toPage ResetPasswordPage ResetPasswordMsg ResetPassword.update resetPasswordMsg resetPasswordModel

        ( ChangePasswordMsg changePasswordMsg, ChangePasswordPage changePasswordModel ) ->
            toPage ChangePasswordPage ChangePasswordMsg ChangePassword.update changePasswordMsg changePasswordModel

        ( StoreChanged json, _ ) ->
            ( { model | session = { session | store = Session.deserializeStore json } }
            , Cmd.none
            )

        ( UrlRequested urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl session.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            setRoute (Route.fromUrl url) model

        ( _, NotFound ) ->
            ( { model | page = NotFound }, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.storeChanged StoreChanged
        , case model.page of
            HomePage _ ->
                Sub.none

            ValidatePage _ ->
                Sub.none

            ResetPasswordPage _ ->
                Sub.none

            ChangePasswordPage _ ->
                Sub.none

            NotFound ->
                Sub.none

            Blank ->
                Sub.none
        ]


view : Model -> Document Msg
view { page, session } =
    let
        pageConfig =
            Page.Config session

        mapMsg msg ( title, content ) =
            ( title, content |> List.map (Html.map msg) )
    in
    case page of
        HomePage homeModel ->
            Home.view session homeModel
                |> mapMsg HomeMsg
                |> Page.frame (pageConfig Page.Home)

        ValidatePage validateModel ->
            Validate.view session validateModel
                |> mapMsg ValidateMsg
                |> Page.frame (pageConfig Page.Validate)

        ResetPasswordPage resetPasswordModel ->
            ResetPassword.view session resetPasswordModel
                |> mapMsg ResetPasswordMsg
                |> Page.frame (pageConfig Page.ResetPassword)

        ChangePasswordPage changePasswordModel ->
            ChangePassword.view session changePasswordModel
                |> mapMsg ChangePasswordMsg
                |> Page.frame (pageConfig Page.ChangePassword)

        NotFound ->
            ( "Not Found", [ Html.text "Not found" ] )
                |> Page.frame (pageConfig Page.Other)

        Blank ->
            ( "", [] )
                |> Page.frame (pageConfig Page.Other)


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }
