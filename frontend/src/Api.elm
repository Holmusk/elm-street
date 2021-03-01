module Api exposing
       ( postOneType
       , getOneType
       , ResultErr
       )

import Http exposing (Error)
import Json.Decode as D

import Core.Decoder exposing (decodeOneType)
import Core.Encoder exposing (encodeOneType)
import Core.Types exposing (OneType)


type alias ResultErr a = Result Error a

getOneType : (ResultErr OneType -> msg) -> Cmd msg
getOneType f = Http.request
    { method  = "GET"
    , headers = []
    , url     = "get"
    , body    = Http.emptyBody
    , expect  = Http.expectJson f decodeOneType
    , timeout = Nothing
    , tracker = Nothing
    }

postOneType : OneType -> (ResultErr Bool -> msg) -> Cmd msg
postOneType req f = Http.request
    { method  = "POST"
    , headers = []
    , url     = "post"
    , body    = Http.jsonBody (encodeOneType req)
    , timeout = Nothing
    , tracker = Nothing
    , expect  = Http.expectJson f D.bool
    }
