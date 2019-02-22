module Core.Decoder exposing (..)

import Iso8601 as Iso
import Json.Decode as D exposing (..)
import Json.Decode.Pipeline as D exposing (required)

import Core.ElmStreet exposing (..)
import Core.Types exposing (..)


decodePrims : Decoder Prims
decodePrims = D.succeed Prims
    |> D.hardcoded ()
    |> required "bool" D.bool
    |> required "char" elmStreetDecodeChar
    |> required "int" D.int
    |> required "float" D.float
    |> required "string" D.string
    |> required "time" Iso.decoder
    |> required "maybe" (nullable D.int)
    |> required "result" (elmStreetDecodeEither D.int D.string)
    |> required "pair" (elmStreetDecodePair elmStreetDecodeChar D.bool)
    |> required "list" (D.list D.int)

decodeId : Decoder Id
decodeId = D.map Id D.string

decodeAge : Decoder Age
decodeAge = D.map Age D.int

decodeRequestStatus : Decoder RequestStatus
decodeRequestStatus = elmStreetDecodeEnum readRequestStatus

decodeUser : Decoder User
decodeUser = D.succeed User
    |> required "id" decodeId
    |> required "name" D.string
    |> required "age" decodeAge
    |> required "status" decodeRequestStatus

decodeGuest : Decoder Guest
decodeGuest =
    let decide : String -> Decoder Guest
        decide x = case x of
            "Regular" -> D.field "contents" <| D.map2 Regular (D.index 0 D.string) (D.index 1 D.int)
            "Visitor" -> D.field "contents" <| D.map Visitor (D.index 0 D.string)
            "Blocked" -> D.succeed Blocked
            c -> D.fail <| "Guest doesn't have such constructor: " ++ c
    in D.andThen decide (D.field "tag" D.string)

decodeUserRequest : Decoder UserRequest
decodeUserRequest = D.succeed UserRequest
    |> required "ids" (D.list decodeId)
    |> required "limit" D.int
    |> required "example" (nullable (elmStreetDecodeEither decodeUser decodeGuest))

decodeOneType : Decoder OneType
decodeOneType = D.succeed OneType
    |> required "prims" decodePrims
    |> required "id" decodeId
    |> required "age" decodeAge
    |> required "requestStatus" decodeRequestStatus
    |> required "user" decodeUser
    |> required "guest" decodeGuest
    |> required "userRequest" decodeUserRequest
