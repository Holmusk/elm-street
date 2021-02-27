module Core.Decoder exposing (..)

import Iso8601 as Iso
import Json.Decode as D exposing (..)
import Json.Decode.Pipeline as D exposing (required)

import Core.ElmStreet exposing (..)
import Core.Types as T


decodePrims : Decoder T.Prims
decodePrims = D.succeed T.Prims
    |> D.hardcoded ()
    |> required "bool" D.bool
    |> required "char" elmStreetDecodeChar
    |> required "int" D.int
    |> required "float" D.float
    |> required "text" D.string
    |> required "time" Iso.decoder
    |> required "value" D.value
    |> required "maybe" (nullable D.int)
    |> required "result" (elmStreetDecodeEither D.int D.string)
    |> required "pair" (elmStreetDecodePair elmStreetDecodeChar D.bool)
    |> required "triple" (elmStreetDecodeTriple elmStreetDecodeChar D.bool (D.list D.int))
    |> required "list" (D.list D.int)

decodeMyUnit : Decoder T.MyUnit
decodeMyUnit =
    let decide : String -> Decoder T.MyUnit
        decide x = case x of
            "MyUnit" -> D.field "contents" <| D.map T.MyUnit (D.map (always ()) (D.list D.string))
            c -> D.fail <| "MyUnit doesn't have such constructor: " ++ c
    in D.andThen decide (D.field "tag" D.string)

decodeMyResult : Decoder T.MyResult
decodeMyResult =
    let decide : String -> Decoder T.MyResult
        decide x = case x of
            "Ok" -> D.succeed T.Ok
            "Err" -> D.field "contents" <| D.map T.Err D.string
            c -> D.fail <| "MyResult doesn't have such constructor: " ++ c
    in D.andThen decide (D.field "tag" D.string)

decodeId : Decoder T.Id
decodeId = D.map T.Id D.string

decodeAge : Decoder T.Age
decodeAge = D.map T.Age D.int

decodeNewtype : Decoder T.Newtype
decodeNewtype = D.map T.Newtype D.int

decodeNewtypeList : Decoder T.NewtypeList
decodeNewtypeList = D.map T.NewtypeList (D.list D.int)

decodeOneConstructor : Decoder T.OneConstructor
decodeOneConstructor = elmStreetDecodeEnum T.readOneConstructor

decodeRequestStatus : Decoder T.RequestStatus
decodeRequestStatus = elmStreetDecodeEnum T.readRequestStatus

decodeUser : Decoder T.User
decodeUser = D.succeed T.User
    |> required "id" decodeId
    |> required "name" D.string
    |> required "age" decodeAge
    |> required "status" decodeRequestStatus

decodeGuest : Decoder T.Guest
decodeGuest =
    let decide : String -> Decoder T.Guest
        decide x = case x of
            "Regular" -> D.field "contents" <| D.map2 T.Regular (D.index 0 D.string) (D.index 1 D.int)
            "Visitor" -> D.field "contents" <| D.map T.Visitor D.string
            "Special" -> D.field "contents" <| D.map T.Special (nullable (D.list D.int))
            "Blocked" -> D.succeed T.Blocked
            c -> D.fail <| "Guest doesn't have such constructor: " ++ c
    in D.andThen decide (D.field "tag" D.string)

decodeUserRequest : Decoder T.UserRequest
decodeUserRequest = D.succeed T.UserRequest
    |> required "ids" (D.list decodeId)
    |> required "limit" D.int
    |> required "example" (nullable (elmStreetDecodeEither decodeUser decodeGuest))

decodeOneType : Decoder T.OneType
decodeOneType = D.succeed T.OneType
    |> required "prims" decodePrims
    |> required "myUnit" decodeMyUnit
    |> required "myResult" decodeMyResult
    |> required "id" decodeId
    |> required "age" decodeAge
    |> required "newtype" decodeNewtype
    |> required "newtypeList" decodeNewtypeList
    |> required "oneConstructor" decodeOneConstructor
    |> required "requestStatus" decodeRequestStatus
    |> required "user" decodeUser
    |> required "guests" (D.list decodeGuest)
    |> required "userRequest" decodeUserRequest
