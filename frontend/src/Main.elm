module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, h2, button)
import Html.Attributes exposing (src, class, disabled)
import Html.Events exposing (onClick)

import Api exposing (ResultErr, getOneType, postOneType)
import Core.Types exposing (OneType)

---- MODEL ----

type alias Model =
    { oneType    : Maybe OneType
    , getErr     : Bool
    , postErr    : Bool
    , postResult : Maybe Bool
    }

init : ( Model, Cmd Msg )
init =
    ( {oneType = Nothing, getErr = False, postErr = False, postResult = Nothing}
    , Cmd.none)

---- UPDATE ----

type Msg
    = NoOp
    | GetOneType
    | GetOneTypeRes (ResultErr OneType)
    | PostOneType (Maybe OneType)
    | PostOneTypeRes (ResultErr Bool)
    | Refresh

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
    NoOp -> ( model, Cmd.none )
    GetOneType -> (model, getOneType GetOneTypeRes)
    GetOneTypeRes res -> case res of
        Ok oneType -> ({model| oneType = Just oneType}, Cmd.none)
        Err err -> ({model| getErr = True}, Cmd.none)
    PostOneType (Just t) -> (model, postOneType t PostOneTypeRes)
    PostOneType _ -> (model, Cmd.none)
    PostOneTypeRes res -> case res of
        Ok isSame -> ({model| postResult = Just isSame}, Cmd.none)
        Err err -> ({model| postErr = True}, Cmd.none)
    Refresh -> init

---- VIEW ----

view : Model -> Html Msg
view m = div []
    [ h1 [] [text "Elm Street testing application"]
    , h2 [] [text "Get 'OneType' endpoint"]
    , button [onClick GetOneType] [text "Get OneType"]
    , div [class "err"] (if m.getErr then [text "Get errored"] else [])
    , h2 [] [text "Get 'OneType' endpoint"]
    , button [onClick (PostOneType m.oneType), disabled (isNothing m.oneType)] [text "Post OneType"]
    , div [class "err"] (if m.postErr then [text "Post errored"] else [])
    , div [] <| case m.postResult of
         Just True -> [text "The 'get' and 'post' OneType is the same :)"]
         Just False -> [text "The 'get' and 'post' OneType are different :("]
         Nothing -> []
    , button [onClick Refresh] [text "Refresh"]
    ]

---- PROGRAM ----

main : Program () Model Msg
main = Browser.element
    { view = view
    , init = \_ -> init
    , update = update
    , subscriptions = always Sub.none
    }

-- Util
isNothing : Maybe a -> Bool
isNothing x = case x of
    Nothing -> True
    _ -> False
