module Tests exposing (..)

import Expect
import Json.Encode as E exposing (encode)
import Json.Decode exposing (decodeString)
import Test exposing (..)
import Time exposing (millisToPosix)
import Result as R

import Core.Decoder exposing (decodeOneType)
import Core.Encoder exposing (encodeOneType)
import Core.Types exposing (..)
import Core.Types as T exposing (MyResult (..))
import Tests.Golden exposing (goldenOneTypeJson)

-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    let oneTypeE : String
        oneTypeE = encode 0 <| encodeOneType defaultOneType
    in
    describe "Encode / Decode Golden Test"
        [ test "Elm Type -> Json -> Elm Type == default" <| \_ -> Expect.equal
            (decodeString decodeOneType oneTypeE)
            (R.Ok defaultOneType)
        , test "Golden Json -> Elm == default" <| \_ -> Expect.equal
            (decodeString decodeOneType goldenOneTypeJson)
            (R.Ok defaultOneType)
        ]

defaultOneType : OneType
defaultOneType =
    let
        guestRegular : Guest
        guestRegular = Regular "nice" 7
        guestVisitor : Guest
        guestVisitor = Visitor "new-guest"
        guestBlocked : Guest
        guestBlocked = Blocked
    in
    { prims =
        { unit     = ()
        , bool     = True
        , char     = 'a'
        , int      = 42
        , float    = 36.6
        , text     = "heh"
        , value  = E.object
            [ ("nullField", E.null)
            , ("boolField", E.bool True)
            , ("numberField", E.int 1)
            , ("stringField", E.string "hi")
            , ("arrayField", E.list E.int [1,2,3])
            , ("objectField", E.object [])
            ]
        , time     = millisToPosix 1550793600000  -- UTCTime (fromGregorian 2019 2 22) 0
        , maybe    = Just 12
        , result   = R.Err 666
        , pair     = ('o', False)
        , triple   = ('o', False, [0])
        , list     = [1, 2, 3, 4, 5]
        , nonEmpty = (1, [])

        }
    , myUnit = MyUnit ()
    , myResult = T.Err "clashing test"
    , id = Id "myId"
    , age = Age 18
    , newtype = Newtype 666
    , newtypeList = NewtypeList [123]
    , oneConstructor = OneConstructor
    , requestStatus = Reviewing
    , user = User (Id "1") "not-me" (Age 100) Approved
    , guests = [guestRegular, guestVisitor, guestBlocked]
    , userRequest =
        { ids     = [Id "1", Id "2"]
        , limit   = 123
        , example = Just (R.Ok Blocked)
        }
    , nonEmpty = (MyUnit (), [MyUnit ()])
    }
