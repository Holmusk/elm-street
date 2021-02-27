{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeOperators       #-}

module Elm.Generate
       ( Settings (..)
       , defaultSettings
       , generateElm

         -- * Internal helpers
       , RenderElm (..)
       ) where

import Data.Kind (Type)
import Data.List (intercalate)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((<.>), (</>))

import Elm.Generic (Elm (..))
import Elm.Print (decodeChar, decodeEither, decodeEnum, decodePair, decodeTriple, encodeEither, encodeMaybe,
                  encodePair, encodeTriple, prettyShowDecoder, prettyShowDefinition, prettyShowEncoder)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO


-- | Settings for outputting generated Elm code.
data Settings = Settings
    { settingsDirectory   :: !FilePath    -- ^ Directory to put generated files, e.g. @frontend\/src@
    , settingsModule      :: ![FilePath]  -- ^ List of module parts, like @["ABC", "Core"]@
    , settingsTypesFile   :: !FilePath    -- ^ File name for module with types, e.g. @Types@
    , settingsEncoderFile :: !FilePath    -- ^ File name for module with JSON encoders, e.g. @Encoder@
    , settingsDecoderFile :: !FilePath    -- ^ File name for module with JSON decoders, e.g. @Decoder@
    }

{- | Default settings for generating Elm definitions. You only need to pass name
of the directory and module path prefix. Other settings parameters set to:

1. 'settingsTypesFile': @Types@
2. 'settingsEncoderFile': @Encoder@
3. 'settingsDecoderFile': @Decoder@
-}
defaultSettings :: FilePath -> [FilePath] -> Settings
defaultSettings settingsDirectory settingsModule = Settings
    { settingsTypesFile   = "Types"
    , settingsEncoderFile = "Encoder"
    , settingsDecoderFile = "Decoder"
    , ..
    }

-- | Typeclass for generating elm definitions for the list of types.
class RenderElm (types :: [Type]) where
    renderType    :: [Text]
    renderEncoder :: [Text]
    renderDecoder :: [Text]

instance RenderElm '[] where
    renderType    = []
    renderEncoder = []
    renderDecoder = []

instance (Elm t, RenderElm ts) => RenderElm (t ': ts) where
    renderType    = "" : toElmTypeSource    @t : renderType    @ts
    renderEncoder = "" : toElmEncoderSource @t : renderEncoder @ts
    renderDecoder = "" : toElmDecoderSource @t : renderDecoder @ts

toElmTypeSource :: forall a . Elm a => Text
toElmTypeSource = prettyShowDefinition $ toElmDefinition $ Proxy @a

toElmEncoderSource :: forall a . Elm a => Text
toElmEncoderSource = prettyShowEncoder $ toElmDefinition $ Proxy @a

toElmDecoderSource :: forall a . Elm a => Text
toElmDecoderSource = prettyShowDecoder $ toElmDefinition $ Proxy @a

{- | Generate elm definitions for the list of types. This function is supposed
to be called like this:

@
__type__ Types =
   '[ User
    , UserStatus
    , Measure
    ]

main :: IO ()
main = generateElm @Types $ defaultSettings "frontend\/src\/" ["ABC", "Core"]
@
-}
generateElm :: forall (ts :: [Type]) . RenderElm ts => Settings -> IO ()
generateElm Settings{..} = do
    createDirectoryIfMissing True fullPath

    writeElm settingsTypesFile   $ typesHeader   : renderType    @ts
    writeElm settingsEncoderFile $ encoderHeader : renderEncoder @ts
    writeElm settingsDecoderFile $ decoderHeader : renderDecoder @ts

    writeElm "ElmStreet" elmStreetDefinitions
  where
    moduleDir, fullPath :: FilePath
    moduleDir = foldr (</>) "" settingsModule
    fullPath  = settingsDirectory </> moduleDir

    writeElm :: FilePath -> [Text] -> IO ()
    writeElm file defs = TIO.writeFile (fullPath </> file <.> "elm") (T.unlines defs)

    joinModule :: [String] -> Text
    joinModule = T.pack . intercalate "."

    typesModule, encoderModule, decoderModule :: Text
    typesModule   = joinModule $ settingsModule ++ [settingsTypesFile]
    encoderModule = joinModule $ settingsModule ++ [settingsEncoderFile]
    decoderModule = joinModule $ settingsModule ++ [settingsDecoderFile]
    streetModule  = joinModule $ settingsModule ++ ["ElmStreet"]

    typesHeader :: Text
    typesHeader = T.unlines
        [ "module " <> typesModule <> " exposing (..)"
        , ""
        , "import Time exposing (Posix)"
        , "import Json.Decode exposing (Value)"
        ]

    encoderHeader :: Text
    encoderHeader = T.unlines
        [ "module " <> encoderModule <> " exposing (..)"
        , ""
        , "import Iso8601 as Iso"
        , "import Json.Encode as E exposing (..)"
        , ""
        , "import " <> streetModule <> " exposing (..)"
        , "import " <> typesModule <> " as T"
        ]

    decoderHeader :: Text
    decoderHeader = T.unlines
        [ "module " <> decoderModule <> " exposing (..)"
        , ""
        , "import Iso8601 as Iso"
        , "import Json.Decode as D exposing (..)"
        , "import Json.Decode.Pipeline as D exposing (required)"
        , ""
        , "import " <> streetModule <> " exposing (..)"
        , "import " <> typesModule <> " as T"
        ]

    elmStreetDefinitions :: [Text]
    elmStreetDefinitions =
        [ "module " <> streetModule <> " exposing (..)"
        , ""
        , "import Json.Encode as E exposing (Value)"
        , "import Json.Decode as D exposing (Decoder)"
        , "import Json.Decode.Pipeline as D exposing (..)"
        , ""
        , ""
        , encodeMaybe
        , encodeEither
        , encodePair
        , encodeTriple

        , decodeEnum
        , decodeChar
        , decodeEither
        , decodePair
        , decodeTriple
        ]
