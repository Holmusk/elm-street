module Core.Types exposing (..)

import Time exposing (Posix)


type alias Prims =
    { unit : ()
    , bool : Bool
    , char : Char
    , int : Int
    , float : Float
    , string : String
    , time : Posix
    , maybe : Maybe Int
    , result : Result Int String
    , pair : (Char, Bool)
    , list : List Int
    }

type alias Id =
    { unId : String
    }

type alias Age =
    { age : Int
    }

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
    | Blocked

type alias UserRequest =
    { ids : List Id
    , limit : Int
    , example : Maybe (Result User Guest)
    }
