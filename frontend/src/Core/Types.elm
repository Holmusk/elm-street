module Core.Types exposing (..)

import Time exposing (Posix)
import Json.Decode exposing (Value)


type alias PrimsRecord =
    { unit : ()
    , bool : Bool
    , char : Char
    , int : Int
    , float : Float
    , text : String
    , string : String
    , time : Posix
    , value : Value
    , maybe : Maybe Int
    , result : Result Int String
    , pair : (Char, Bool)
    , triple : (Char, Bool, List Int)
    , list : List Int
    , nonEmpty : (Int, List Int)
    }

type Prims
    = Prims PrimsRecord

type MyUnit
    = MyUnit ()

type MyResult
    = Ok
    | Err String

type Id a
    = Id String

unId : Id a -> String
unId (Id x) = x

type Ref a
    = Ref String

unRef : Ref a -> String
unRef (Ref x) = x

type alias AgeRecord =
    { age : Int
    }

type Age
    = Age AgeRecord

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

type alias UserRecord =
    { id : Id User
    , name : String
    , age : Age
    , status : RequestStatus
    }

type User
    = User UserRecord

type Guest
    = Regular String Int
    | Visitor String
    | Special (Maybe (List Int))
    | Blocked

type alias UserRequestRecord =
    { ids : List (Id User)
    , limit : Int
    , example : Maybe (Result User Guest)
    }

type UserRequest
    = UserRequest UserRequestRecord

type alias OneTypeRecord =
    { prims : Prims
    , myUnit : MyUnit
    , myResult : MyResult
    , id : Id OneType
    , age : Age
    , newtype : Newtype
    , newtypeList : NewtypeList
    , oneConstructor : OneConstructor
    , requestStatus : RequestStatus
    , user : User
    , guests : List Guest
    , userRequest : UserRequest
    , nonEmpty : (MyUnit, List MyUnit)
    }

type OneType
    = OneType OneTypeRecord

type alias CustomCodeGenRecord =
    { customFunTestString : String
    , customFunTestInt : Int
    }

type CustomCodeGen
    = CustomCodeGen CustomCodeGenRecord
