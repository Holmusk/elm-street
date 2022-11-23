module Core.Encoder exposing (..)

import Iso8601 as Iso
import Json.Encode as E exposing (..)

import Core.ElmStreet exposing (..)
import Core.Types as T


encodePrims : T.Prims -> Value
encodePrims x = E.object
    [ ("tag", E.string "Prims")
    , ("unit", (always <| E.list identity []) x.unit)
    , ("bool", E.bool x.bool)
    , ("char", (E.string << String.fromChar) x.char)
    , ("int", E.int x.int)
    , ("float", E.float x.float)
    , ("text", E.string x.text)
    , ("time", Iso.encode  x.time)
    , ("value", Basics.identity  x.value)
    , ("maybe", (elmStreetEncodeMaybe E.int) x.maybe)
    , ("result", (elmStreetEncodeEither E.int E.string) x.result)
    , ("pair", (elmStreetEncodePair (E.string << String.fromChar) E.bool) x.pair)
    , ("triple", (elmStreetEncodeTriple (E.string << String.fromChar) E.bool (E.list E.int)) x.triple)
    , ("list", (E.list E.int) x.list)
    , ("nonEmpty", (elmStreetEncodeNonEmpty E.int) x.nonEmpty)
    ]

encodeMyUnit : T.MyUnit -> Value
encodeMyUnit x = E.object <| case x of
    T.MyUnit x1 -> [("tag", E.string "MyUnit"), ("contents", (always <| E.list identity []) x1)]

encodeMyResult : T.MyResult -> Value
encodeMyResult x = E.object <| case x of
    T.Ok  -> [("tag", E.string "Ok"), ("contents", E.list identity [])]
    T.Err x1 -> [("tag", E.string "Err"), ("contents", E.string x1)]

encodeId : T.Id -> Value
encodeId x = E.string x.unId

encodeAge : T.Age -> Value
encodeAge x = E.int x.age

encodeNewtype : T.Newtype -> Value
encodeNewtype = E.int << T.unNewtype

encodeNewtypeList : T.NewtypeList -> Value
encodeNewtypeList = (E.list E.int) << T.unNewtypeList

encodeOneConstructor : T.OneConstructor -> Value
encodeOneConstructor = E.string << T.showOneConstructor

encodeRequestStatus : T.RequestStatus -> Value
encodeRequestStatus = E.string << T.showRequestStatus

encodeUser : T.User -> Value
encodeUser x = E.object
    [ ("tag", E.string "User")
    , ("id", encodeId x.id)
    , ("name", E.string x.name)
    , ("age", encodeAge x.age)
    , ("status", encodeRequestStatus x.status)
    ]

encodeGuest : T.Guest -> Value
encodeGuest x = E.object <| case x of
    T.Regular x1 x2 -> [("tag", E.string "Regular"), ("contents", E.list identity [E.string x1, E.int x2])]
    T.Visitor x1 -> [("tag", E.string "Visitor"), ("contents", E.string x1)]
    T.Special x1 -> [("tag", E.string "Special"), ("contents", (elmStreetEncodeMaybe (E.list E.int)) x1)]
    T.Blocked  -> [("tag", E.string "Blocked"), ("contents", E.list identity [])]

encodeUserRequest : T.UserRequest -> Value
encodeUserRequest x = E.object
    [ ("tag", E.string "UserRequest")
    , ("ids", (E.list encodeId) x.ids)
    , ("limit", E.int x.limit)
    , ("example", (elmStreetEncodeMaybe (elmStreetEncodeEither encodeUser encodeGuest)) x.example)
    ]

encodeOneType : T.OneType -> Value
encodeOneType x = E.object
    [ ("tag", E.string "OneType")
    , ("prims", encodePrims x.prims)
    , ("myUnit", encodeMyUnit x.myUnit)
    , ("myResult", encodeMyResult x.myResult)
    , ("id", encodeId x.id)
    , ("age", encodeAge x.age)
    , ("newtype", encodeNewtype x.newtype)
    , ("newtypeList", encodeNewtypeList x.newtypeList)
    , ("oneConstructor", encodeOneConstructor x.oneConstructor)
    , ("requestStatus", encodeRequestStatus x.requestStatus)
    , ("user", encodeUser x.user)
    , ("guests", (E.list encodeGuest) x.guests)
    , ("userRequest", encodeUserRequest x.userRequest)
    , ("nonEmpty", (elmStreetEncodeNonEmpty encodeMyUnit) x.nonEmpty)
    ]
