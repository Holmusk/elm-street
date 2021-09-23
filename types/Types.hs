{-# LANGUAGE CPP                #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
#if ( __GLASGOW_HASKELL__ >= 806 )
{-# LANGUAGE DerivingVia        #-}
#endif
{-# LANGUAGE DerivingStrategies #-}

{- | Haskell types used for testing `elm-street` generated Elm types.
-}

module Types
       ( Types
       , OneType (..)
       , defaultOneType

         -- * All test types
       , Prims (..)
       , Id (..)
       , Age (..)
       , Newtype (..)
       , NewtypeList (..)
       , OneConstructor (..)
       , RequestStatus (..)
       , User (..)
       , Guest (..)
       , UserRequest (..)
       ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, (.=))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Data.Word (Word32)
import Elm (Elm (..), ElmDefinition (..), ElmRecord (..), ElmStreet (..), elmNewtype,
            elmStreetParseJson, elmStreetToJson)
import GHC.Generics (Generic)


data Prims = Prims
    { primsUnit     :: !()
    , primsBool     :: !Bool
    , primsChar     :: !Char
    , primsInt      :: !Int
    , primsFloat    :: !Double
    , primsText     :: !Text
    , primsTime     :: !UTCTime
    , primsValue    :: !Value
    , primsMaybe    :: !(Maybe Word)
    , primsResult   :: !(Either Int Text)
    , primsPair     :: !(Char, Bool)
    , primsTriple   :: !(Char, Bool, [Int])
    , primsList     :: ![Int]
    , primsNonEmpty :: !(NonEmpty Int)
    } deriving (Generic, Eq, Show)
#if ( __GLASGOW_HASKELL__ >= 806 )
      deriving (Elm, ToJSON, FromJSON) via ElmStreet Prims
#else
      deriving anyclass Elm

instance ToJSON   Prims where toJSON = elmStreetToJson
instance FromJSON Prims where parseJSON = elmStreetParseJson
#endif

newtype Id a = Id
    { unId :: Text
    } deriving (Show, Eq)
      deriving newtype (FromJSON, ToJSON)

instance Elm (Id a) where
    toElmDefinition _ = elmNewtype @Text "Id" "unId"

newtype Age = Age
    { unAge :: Int
    } deriving (Generic, Eq, Show)
      deriving anyclass (Elm)
      deriving newtype (FromJSON, ToJSON)

newtype Newtype = Newtype Int
    deriving stock (Generic, Eq, Show)
    deriving newtype (FromJSON, ToJSON)
    deriving anyclass (Elm)

newtype NewtypeList = NewtypeList [Int]
    deriving stock (Generic, Eq, Show)
    deriving newtype (FromJSON, ToJSON)
    deriving anyclass (Elm)

data OneConstructor = OneConstructor
    deriving stock (Generic, Eq, Show)
    deriving anyclass (Elm)

instance ToJSON   OneConstructor where toJSON = elmStreetToJson
instance FromJSON OneConstructor where parseJSON = elmStreetParseJson

data RequestStatus
    = Approved
    | Rejected
    | Reviewing
    deriving (Generic, Eq, Show)
    deriving anyclass (Elm)

instance ToJSON   RequestStatus where toJSON = elmStreetToJson
instance FromJSON RequestStatus where parseJSON = elmStreetParseJson

data User = User
    { userId     :: !(Id User)
    , userName   :: !Text
    , userAge    :: !Age
    , userStatus :: !RequestStatus
    } deriving (Generic, Eq, Show)
      deriving anyclass (Elm)

instance ToJSON   User where toJSON = elmStreetToJson
instance FromJSON User where parseJSON = elmStreetParseJson

data Guest
    = Regular Text Int
    | Visitor Text
    | Special (Maybe [Int])
    | Blocked
    deriving (Generic, Eq, Show)
    deriving anyclass (Elm)

instance ToJSON   Guest where toJSON = elmStreetToJson
instance FromJSON Guest where parseJSON = elmStreetParseJson

data UserRequest = UserRequest
    { userRequestIds     :: ![Id User]
    , userRequestLimit   :: !Word32
    , userRequestExample :: !(Maybe (Either User Guest))
    } deriving (Generic, Eq, Show)
      deriving anyclass (Elm)

instance ToJSON   UserRequest where toJSON = elmStreetToJson
instance FromJSON UserRequest where parseJSON = elmStreetParseJson

data MyUnit = MyUnit ()
    deriving stock (Show, Eq, Ord, Generic)
#if ( __GLASGOW_HASKELL__ >= 806 )
      deriving (Elm, ToJSON, FromJSON) via ElmStreet MyUnit
#else
      deriving anyclass Elm

instance ToJSON   MyUnit where toJSON = elmStreetToJson
instance FromJSON MyUnit where parseJSON = elmStreetParseJson
#endif

-- | For name clashes testing.
data MyResult
    = Ok
    | Err Text
    deriving stock (Generic, Eq, Show)
    deriving anyclass (Elm)

instance ToJSON   MyResult where toJSON = elmStreetToJson
instance FromJSON MyResult where parseJSON = elmStreetParseJson

data RecordUnit = RecordUnit
    deriving stock (Generic, Eq, Show)
    deriving anyclass (FromJSON, ToJSON)

instance Elm RecordUnit where
    toElmDefinition _ = DefRecord $ ElmRecord
                            { elmRecordName = "RecordUnit"
                            , elmRecordFields = []
                            , elmRecordIsNewtype = False
                            }


-- | All test types together in one type to play with.
data OneType = OneType
    { oneTypePrims          :: !Prims
    , oneTypeMyUnit         :: !MyUnit
    , oneTypeMyResult       :: !MyResult
    , oneTypeId             :: !(Id OneType)
    , oneTypeAge            :: !Age
    , oneTypeNewtype        :: !Newtype
    , oneTypeNewtypeList    :: !NewtypeList
    , oneTypeOneConstructor :: !OneConstructor
    , oneTypeRequestStatus  :: !RequestStatus
    , oneTypeUser           :: !User
    , oneTypeGuests         :: ![Guest]
    , oneTypeUserRequest    :: !UserRequest
    , oneTypeNonEmpty       :: !(NonEmpty MyUnit)
    , oneTypeRecordUnit     :: !RecordUnit
    } deriving (Generic, Eq, Show)
      deriving anyclass (Elm)

instance ToJSON   OneType where toJSON = elmStreetToJson
instance FromJSON OneType where parseJSON = elmStreetParseJson

-- | Type level list of all test types.
type Types =
   '[ Prims
    , MyUnit
    , MyResult
    , Id ()
    , Age
    , Newtype
    , NewtypeList
    , OneConstructor
    , RequestStatus
    , User
    , Guest
    , UserRequest
    , OneType
    , RecordUnit
    ]


defaultOneType :: OneType
defaultOneType = OneType
    { oneTypePrims = defaultPrims
    , oneTypeMyUnit = MyUnit ()
    , oneTypeMyResult = Err "clashing test"
    , oneTypeId = Id "myId"
    , oneTypeAge = Age 18
    , oneTypeNewtype = Newtype 666
    , oneTypeNewtypeList = NewtypeList [123]
    , oneTypeOneConstructor = OneConstructor
    , oneTypeRequestStatus = Reviewing
    , oneTypeUser = User (Id "1") "not-me" (Age 100) Approved
    , oneTypeGuests = [guestRegular, guestVisitor, guestBlocked]
    , oneTypeUserRequest = defaultUserRequest
    , oneTypeNonEmpty = MyUnit () :| [ MyUnit () ]
    , oneTypeRecordUnit = RecordUnit
    }
  where
    defaultPrims :: Prims
    defaultPrims = Prims
        { primsUnit     = ()
        , primsBool     = True
        , primsChar     = 'a'
        , primsInt      = 42
        , primsFloat    = 36.6
        , primsText     = "heh"
        , primsValue    = object
            [ "nullField"   .= Null
            , "boolField"   .= True
            , "numberField" .= (1::Int)
            , "stringField" .= ("hi"::String)
            , "arrayField"  .= [1::Int,2,3]
            , "objectField" .= object []
            ]
        , primsTime     = UTCTime (fromGregorian 2019 2 22) 0
        , primsMaybe    = Just 12
        , primsResult   = Left 666
        , primsPair     = ('o', False)
        , primsTriple   = ('o', False, [0])
        , primsList     = [1..5]
        , primsNonEmpty = 1 :| []
        }

    guestRegular, guestVisitor, guestBlocked :: Guest
    guestRegular = Regular "nice" 7
    guestVisitor = Visitor "new-guest"
    guestBlocked = Blocked

    defaultUserRequest :: UserRequest
    defaultUserRequest = UserRequest
        { userRequestIds     = [Id "1", Id "2"]
        , userRequestLimit   = 123
        , userRequestExample = Just (Right Blocked)
        }
