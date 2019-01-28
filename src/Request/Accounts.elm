module Request.Accounts exposing (createAccount, errorToString, validateKey)

import Data.Session exposing (Session)
import Http exposing (Error(..))
import Json.Encode as Encode


type alias Login =
    String


type alias Password =
    String


type alias Key =
    String


errorToString : Http.Error -> String
errorToString error =
    case error of
        BadUrl _ ->
            "Bad url."

        Timeout ->
            "Request timed out."

        NetworkError ->
            "Network error. Are you online?"

        BadStatus status ->
            "HTTP error " ++ String.fromInt status

        BadBody body ->
            "Unable to parse response body: " ++ body


createAccount : (Result Http.Error () -> msg) -> Session -> Login -> Password -> Cmd msg
createAccount msg session login password =
    Http.post
        { url = session.store.server ++ "/accounts"
        , body = encodeUser login password |> Http.jsonBody
        , expect = Http.expectWhatever msg
        }


encodeUser : Login -> Password -> Encode.Value
encodeUser login password =
    Encode.object
        [ ( "data"
          , Encode.object
                [ ( "id", Encode.string login )
                , ( "password", Encode.string password )
                ]
          )
        ]


validateKey : (Result Http.Error () -> msg) -> Session -> Login -> Key -> Cmd msg
validateKey msg session login key =
    Http.post
        { url = session.store.server ++ "/accounts/" ++ login ++ "/validate/" ++ key
        , body = Http.emptyBody
        , expect = Http.expectWhatever msg
        }
