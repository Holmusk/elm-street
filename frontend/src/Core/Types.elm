module Core.Types exposing (..)

import Time exposing (Posix)
import Json.Decode exposing (Value)


type alias Prims =
    { unit : ()
    , bool : Bool
    , char : Char
    , int : Int
    , float : Float
    , text : String
    , time : Posix
    , value : Value
    , maybe : Maybe Int
    , result : Result Int String
    , pair : (Char, Bool)
    , triple : (Char, Bool, List Int)
    , list : List Int
    }

type MyUnit
    = MyUnit ()

type MyResult
    = Ok
    | Err String

type alias Id =
    { unId : String
    }

type alias Age =
    { age : Int
    }

type Newtype
    = Newtype Int

unNewtype : Newtype -> Int
unNewtype (Newtype x) = x

type NewtypeList
    = NewtypeList (List Int)

unNewtypeList : NewtypeList -> List Int
unNewtypeList (NewtypeList x) = x

type OneConstructor
    = OneConstructor

showOneConstructor : OneConstructor -> String
showOneConstructor x = case x of
    OneConstructor -> "OneConstructor"

readOneConstructor : String -> Maybe OneConstructor
readOneConstructor x = case x of
    "OneConstructor" -> Just OneConstructor
    _ -> Nothing

universeOneConstructor : List OneConstructor
universeOneConstructor = [OneConstructor]

type RequestStatus
    = Approved
    | Rejected
    | Reviewing

showRequestStatus : RequestStatus -> String
showRequestStatus x = case x of
    Approved -> "Approved"
    Rejected -> "Rejected"
    Reviewing -> "Reviewing"

readRequestStatus : String -> Maybe RequestStatus
readRequestStatus x = case x of
    "Approved" -> Just Approved
    "Rejected" -> Just Rejected
    "Reviewing" -> Just Reviewing
    _ -> Nothing

universeRequestStatus : List RequestStatus
universeRequestStatus = [Approved, Rejected, Reviewing]

type alias User =
    { id : Id
    , name : String
    , age : Age
    , status : RequestStatus
    }

type Guest
    = Regular String Int
    | Visitor String
    | Special (Maybe (List Int))
    | Blocked

type alias UserRequest =
    { ids : List Id
    , limit : Int
    , example : Maybe (Result User Guest)
    }

type alias OneType =
    { prims : Prims
    , myUnit : MyUnit
    , myResult : MyResult
    , id : Id
    , age : Age
    , newtype : Newtype
    , newtypeList : NewtypeList
    , oneConstructor : OneConstructor
    , requestStatus : RequestStatus
    , user : User
    , guests : List Guest
    , userRequest : UserRequest
    }
