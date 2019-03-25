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
       , RequestStatus (..)
       , User (..)
       , Guest (..)
       , UserRequest (..)
       ) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Data.Word (Word32)
import Elm (Elm (..), ElmStreet (..), elmNewtype, elmStreetParseJson, elmStreetToJson)
import GHC.Generics (Generic)


data Prims = Prims
    { primsUnit   :: !()
    , primsBool   :: !Bool
    , primsChar   :: !Char
    , primsInt    :: !Int
    , primsFloat  :: !Double
    , primsString :: !String
    , primsTime   :: !UTCTime
    , primsMaybe  :: !(Maybe Word)
    , primsResult :: !(Either Int String)
    , primsPair   :: !(Char, Bool)
    , primsList   :: ![Int]
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

data RequestStatus
    = Approved
    | Rejected
    | Reviewing
    deriving (Generic, Eq, Show)
    deriving anyclass (Elm, FromJSON, ToJSON)

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
    | Blocked
    deriving (Generic, Eq, Show)
    deriving anyclass (Elm, FromJSON, ToJSON)

data UserRequest = UserRequest
    { userRequestIds     :: ![Id User]
    , userRequestLimit   :: !Word32
    , userRequestExample :: !(Maybe (Either User Guest))
    } deriving (Generic, Eq, Show)
      deriving anyclass (Elm)

instance ToJSON   UserRequest where toJSON = elmStreetToJson
instance FromJSON UserRequest where parseJSON = elmStreetParseJson

-- | All test types together in one type to play with.
data OneType = OneType
    { oneTypePrims         :: !Prims
    , oneTypeId            :: !(Id OneType)
    , oneTypeAge           :: !Age
    , oneTypeRequestStatus :: !RequestStatus
    , oneTypeUser          :: !User
    , oneTypeGuests        :: ![Guest]
    , oneTypeUserRequest   :: !UserRequest
    } deriving (Generic, Eq, Show)
      deriving anyclass (Elm)

instance ToJSON   OneType where toJSON = elmStreetToJson
instance FromJSON OneType where parseJSON = elmStreetParseJson

-- | Type level list of all test types.
type Types =
   '[ Prims
    , Id ()
    , Age
    , RequestStatus
    , User
    , Guest
    , UserRequest
    , OneType
    ]


defaultOneType :: OneType
defaultOneType = OneType
    { oneTypePrims = defaultPrims
    , oneTypeId = Id "myId"
    , oneTypeAge = Age 18
    , oneTypeRequestStatus = Reviewing
    , oneTypeUser = User (Id "1") "not-me" (Age 100) Approved
    , oneTypeGuests = [guestRegular, guestVisitor, guestBlocked]
    , oneTypeUserRequest = defaultUserRequest
    }
  where
    defaultPrims :: Prims
    defaultPrims = Prims
        { primsUnit   = ()
        , primsBool   = True
        , primsChar   = 'a'
        , primsInt    = 42
        , primsFloat  = 36.6
        , primsString = "heh"
        , primsTime   = UTCTime (fromGregorian 2019 2 22) 0
        , primsMaybe  = Just 12
        , primsResult = Left 666
        , primsPair   = ('o', False)
        , primsList   = [1..5]
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
