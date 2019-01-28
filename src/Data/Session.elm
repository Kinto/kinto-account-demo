module Data.Session exposing
    ( Session
    , Store
    , deserializeStore
    , serializeStore
    )

import Browser.Navigation as Nav
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias Session =
    { navKey : Nav.Key
    , clientUrl : String
    , store : Store
    }


kintoServer : String
kintoServer =
    "http://localhost:8888/v1"


{-| A serializable data structure holding session information you want to share
across browser restarts, typically in localStorage.
-}
type alias Store =
    { server : String
    , login : String
    }


defaultStore : Store
defaultStore =
    { server = kintoServer
    , login = ""
    }


decodeStore : Decoder Store
decodeStore =
    Decode.map2 Store
        (Decode.field "server" Decode.string)
        (Decode.field "login" Decode.string)


encodeStore : Store -> Encode.Value
encodeStore v =
    Encode.object
        [ ( "server", Encode.string v.server )
        , ( "login", Encode.string v.login )
        ]


deserializeStore : String -> Store
deserializeStore =
    Decode.decodeString decodeStore >> Result.withDefault defaultStore


serializeStore : Store -> String
serializeStore =
    encodeStore >> Encode.encode 0
