module Core.Encoder exposing (..)

import Iso8601 as Iso
import Json.Encode as E exposing (..)

import Core.ElmStreet exposing (..)
import Core.Types exposing (..)


encodePrims : Prims -> Value
encodePrims x = E.object
    [ ("unit", (always E.null) x.unit)
    , ("bool", E.bool x.bool)
    , ("char", (E.string << String.fromChar) x.char)
    , ("int", E.int x.int)
    , ("float", E.float x.float)
    , ("string", E.string x.string)
    , ("time", Iso.encode x.time)
    , ("maybe", (elmStreetEncodeMaybe E.int) x.maybe)
    , ("result", (elmStreetEncodeEither E.int E.string) x.result)
    , ("pair", (elmStreetEncodePair (E.string << String.fromChar) E.bool) x.pair)
    , ("list", E.list E.int x.list)
    ]

encodeId : Id -> Value
encodeId x = E.string x.unId

encodeAge : Age -> Value
encodeAge x = E.int x.age

encodeRequestStatus : RequestStatus -> Value
encodeRequestStatus = E.string << showRequestStatus

encodeUser : User -> Value
encodeUser x = E.object
    [ ("id", encodeId x.id)
    , ("name", E.string x.name)
    , ("age", encodeAge x.age)
    , ("status", encodeRequestStatus x.status)
    ]

encodeGuest : Guest -> Value
encodeGuest x = E.object <| case x of
    Regular x1 x2 -> [("tag", E.string "Regular"), ("contents", E.list identity [E.string x1, E.int x2])]
    Visitor x1 -> [("tag", E.string "Visitor"), ("contents", E.list identity [E.string x1])]
    Blocked  -> [("tag", E.string "Blocked"), ("contents", E.list identity [])]

encodeUserRequest : UserRequest -> Value
encodeUserRequest x = E.object
    [ ("ids", E.list encodeId x.ids)
    , ("limit", E.int x.limit)
    , ("example", (elmStreetEncodeMaybe (elmStreetEncodeEither encodeUser encodeGuest)) x.example)
    ]
