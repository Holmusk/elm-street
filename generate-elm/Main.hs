{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Word (Word32)
import Elm (Elm (..), ElmConstructor (..), ElmDefinition (..), ElmPrim (ElmString), ElmType (..),
            TypeRef (..), defaultSettings, generateElm)
import GHC.Generics (Generic)


data Prims = Prims
    { primsUnit   :: ()
    , primsBool   :: Bool
    , primsChar   :: Char
    , primsInt    :: Int
    , primsFloat  :: Double
    , primsString :: String
    , primsTime   :: UTCTime
    , primsMaybe  :: Maybe Word
    , primsResult :: Either Int String
    , primsPair   :: (Char, Bool)
    , primsList   :: [Int]
    } deriving (Generic)
      deriving anyclass (Elm)

newtype Id a = Id
    { unId :: Text
    } deriving (Show)

instance Elm (Id a) where
    toElmDefinition _ = DefType
        $ ElmType "Id" ["a"] True
        $ ElmConstructor "Id" [RefPrim ElmString] :| []

newtype Age = Age
    { unAge :: Int
    } deriving (Generic)
      deriving anyclass (Elm)

data RequestStatus
    = Approved
    | Rejected
    | Reviewing
    deriving (Generic)
    deriving anyclass (Elm)

data User = User
    { userId     :: Id User
    , userName   :: Text
    , userAge    :: Age
    , userStatus :: RequestStatus
    } deriving (Generic)
      deriving anyclass (Elm)

data Guest
    = Regular Text Int
    | Visitor Text
    | Blocked
    deriving (Generic)
    deriving anyclass (Elm)

data UserRequest = UserRequest
    { urIds     :: [Id User]
    , urLimit   :: Word32
    , urExample :: Maybe (Either User Guest)
    } deriving (Generic)
      deriving anyclass (Elm)

type Types =
   '[ Prims
    , Id ()
    , Age
    , RequestStatus
    , User
    , Guest
    , UserRequest
    ]

main :: IO ()
main = generateElm @Types $ defaultSettings "example" ["Test"]
