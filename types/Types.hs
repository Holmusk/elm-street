{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Haskell types used for testing `elm-street` generated Elm types.
-}

module Types
       ( Types
       , OneType (..)
       , defaultOneType
       , defaultCustomCodeGen

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
       , CustomCodeGen (..)
       ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), object, (.=), GFromJSON, GToJSON, Zero)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Data.Word (Word32)
import Elm (Elm (..), ElmStreet (..), elmNewtype)
import Elm.Generic (CodeGenOptions (..), ElmStreetGenericConstraints, GenericElmDefinition(..))
import Elm.Aeson (elmStreetParseJsonWith, elmStreetToJsonWith)
import GHC.Generics (Generic, Rep)

import qualified GHC.Generics as Generic (from)
import qualified Data.Text as Text

data Prims = Prims
    { primsUnit     :: !()
    , primsBool     :: !Bool
    , primsChar     :: !Char
    , primsInt      :: !Int
    , primsFloat    :: !Double
    , primsText     :: !Text
    , primsString   :: !String
    , primsTime     :: !UTCTime
    , primsValue    :: !Value
    , primsMaybe    :: !(Maybe Word)
    , primsResult   :: !(Either Int Text)
    , primsPair     :: !(Char, Bool)
    , primsTriple   :: !(Char, Bool, [Int])
    , primsList     :: ![Int]
    , primsNonEmpty :: !(NonEmpty Int)
    } deriving (Generic, Eq, Show)
      deriving (Elm, ToJSON, FromJSON) via ElmStreet Prims

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
    deriving (Elm, FromJSON, ToJSON) via ElmStreet OneConstructor

data RequestStatus
    = Approved
    | Rejected
    | Reviewing
    deriving (Generic, Eq, Show)
    deriving (Elm, FromJSON, ToJSON) via ElmStreet RequestStatus

data User = User
    { userId     :: !(Id User)
    , userName   :: !Text
    , userAge    :: !Age
    , userStatus :: !RequestStatus
    } deriving (Generic, Eq, Show)
      deriving (Elm, FromJSON, ToJSON) via ElmStreet User

data Guest
    = Regular Text Int
    | Visitor Text
    | Special (Maybe [Int])
    | Blocked
    deriving (Generic, Eq, Show)
    deriving (Elm, FromJSON, ToJSON) via ElmStreet Guest

data UserRequest = UserRequest
    { userRequestIds     :: ![Id User]
    , userRequestLimit   :: !Word32
    , userRequestExample :: !(Maybe (Either User Guest))
    } deriving (Generic, Eq, Show)
      deriving (Elm, FromJSON, ToJSON) via ElmStreet UserRequest

data MyUnit = MyUnit ()
    deriving stock (Show, Eq, Ord, Generic)
    deriving (Elm, ToJSON, FromJSON) via ElmStreet MyUnit

-- | For name clashes testing.
data MyResult
    = Ok
    | Err Text
    deriving (Generic, Eq, Show)
    deriving (Elm, FromJSON, ToJSON) via ElmStreet MyResult

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
    } deriving (Generic, Eq, Show)
      deriving (Elm, FromJSON, ToJSON) via ElmStreet OneType

data CustomCodeGen = CustomCodeGen
    { customCodeGenString :: String
    , customCodeGenInt :: Int
    } deriving stock (Generic, Eq, Show)
      deriving (Elm, FromJSON, ToJSON) via CustomElm CustomCodeGen

-- Settings which do some custom modifications of record filed names
customCodeGenOptions :: CodeGenOptions
customCodeGenOptions = CodeGenOptions (Text.replace "CodeGen" "FunTest")

-- Newtype whose Elm/ToJSON/FromJSON instance use custom CodeGenOptions
newtype CustomElm a = CustomElm {unCustomElm :: a}

instance ElmStreetGenericConstraints a => Elm (CustomElm a) where
    toElmDefinition _ = genericToElmDefinition customCodeGenOptions
        $ Generic.from (error "Proxy for generic elm was evaluated" :: a)

instance (Generic a, GToJSON Zero (Rep a)) => ToJSON (CustomElm a) where
    toJSON = elmStreetToJsonWith customCodeGenOptions . unCustomElm

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (CustomElm a) where
    parseJSON = fmap CustomElm . elmStreetParseJsonWith customCodeGenOptions

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
    , CustomCodeGen
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
        , primsString   = "bye"
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

defaultCustomCodeGen :: CustomCodeGen
defaultCustomCodeGen = CustomCodeGen
    { customCodeGenString = "Hello"
    , customCodeGenInt = 78
    }
