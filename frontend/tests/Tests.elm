module Tests exposing (..)

import Expect
import Http
import Json.Encode exposing (encode)
import Json.Decode exposing (decodeString)
import Task exposing (Task)
import Test exposing (..)
import Time exposing (millisToPosix)

import Core.Decoder exposing (decodeOneType)
import Core.Encoder exposing (encodeOneType)
import Core.Types exposing (..)

-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


all : Test
all =
    let oneTypeE : String
        oneTypeE = encode 0 <| encodeOneType defaultOneType
    in
    describe "A Test Suite"
        [ test "Elm Type -> Json -> Elm Type == default" <| \_ -> Expect.equal
            (Ok defaultOneType)
            (decodeString decodeOneType oneTypeE)
        ]

-- goldenOneType : Task Http.Error OneType
-- goldenOneType = Http.get decodeOneType
--     ("https://raw.githubusercontent.com/Holmusk/elm-street/master/test/golden/oneType.json")

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
        { unit   = ()
        , bool   = True
        , char   = 'a'
        , int    = 42
        , float  = 36.6
        , string = "heh"
        , time   = millisToPosix 1550779200000 -- UTCTime (fromGregorian 2019 2 22) 0
        , maybe  = Just 12
        , result = Err 666
        , pair   = ('o', False)
        , list   = [1, 2, 3, 4, 5]
        }
    , myUnit = MyUnit ()
    , id = Id "myId"
    , age = Age 18
    , requestStatus = Reviewing
    , user = User (Id "1") "not-me" (Age 100) Approved
    , guests = [guestRegular, guestVisitor, guestBlocked]
    , userRequest =
        { ids     = [Id "1", Id "2"]
        , limit   = 123
        , example = Just (Ok Blocked)
        }
    }
