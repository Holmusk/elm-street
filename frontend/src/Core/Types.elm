module Core.Types exposing (..)

import Time exposing (Posix)
import Json.Decode exposing (Value)

type Prims_ = Prims_
type MyUnit_ = MyUnit_
type MyResult_ = MyResult_
type Id_ = Id_
type Ref_ = Ref_
type Age_ = Age_
type Newtype_ = Newtype_
type NewtypeList_ = NewtypeList_
type OneConstructor_ = OneConstructor_
type RequestStatus_ = RequestStatus_
type User_ = User_
type Guest_ = Guest_
type UserRequest_ = UserRequest_
type OneType_ = OneType_
type CustomCodeGen_ = CustomCodeGen_

type alias Prims =
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
    { id : Id User_
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
    { ids : List (Id User_)
    , limit : Int
    , example : Maybe (Result User Guest)
    }

type alias OneType =
    { prims : Prims
    , myUnit : MyUnit
    , myResult : MyResult
    , id : Id OneType_
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

type alias CustomCodeGen =
    { customFunTestString : String
    , customFunTestInt : Int
    }
