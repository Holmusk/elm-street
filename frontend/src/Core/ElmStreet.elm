module Core.ElmStreet exposing (..)

import Json.Encode as E exposing (Value)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D exposing (..)


elmStreetEncodeMaybe : (a -> Value) -> Maybe a -> Value
elmStreetEncodeMaybe enc = Maybe.withDefault E.null << Maybe.map enc

elmStreetEncodeEither : (a -> Value) -> (b -> Value) -> Result a b -> Value
elmStreetEncodeEither encA encB res = E.object <| case res of
    Err a -> [("Left",  encA a)]
    Ok b  -> [("Right", encB b)]

elmStreetEncodePair : (a -> Value) -> (b -> Value) -> (a, b) -> Value
elmStreetEncodePair encA encB (a, b) = E.list identity [encA a, encB b]

elmStreetEncodeTriple : (a -> Value) -> (b -> Value) -> (c -> Value) -> (a, b, c) -> Value
elmStreetEncodeTriple encA encB encC (a, b, c) = E.list identity [encA a, encB b, encC c]

elmStreetEncodeNonEmpty : (a -> Value) -> (a, List a) -> Value
elmStreetEncodeNonEmpty encA (a, xs) = E.list encA <| a :: xs

decodeStr : (String -> Maybe a) -> String -> Decoder a
decodeStr readX x = case readX x of
    Just a  -> D.succeed a
    Nothing -> D.fail "Constructor not matched"

elmStreetDecodeEnum : (String -> Maybe a) -> Decoder a
elmStreetDecodeEnum r = D.andThen (decodeStr r) D.string

elmStreetDecodeChar : Decoder Char
elmStreetDecodeChar = D.andThen (decodeStr (Maybe.map Tuple.first << String.uncons)) D.string

elmStreetDecodeEither : Decoder a -> Decoder b -> Decoder (Result a b)
elmStreetDecodeEither decA decB = D.oneOf 
    [ D.field "Left"  (D.map Err decA)
    , D.field "Right" (D.map Ok decB)
    ]

elmStreetDecodePair : Decoder a -> Decoder b -> Decoder (a, b)
elmStreetDecodePair decA decB = D.map2 Tuple.pair (D.index 0 decA) (D.index 1 decB)

elmStreetDecodeTriple : Decoder a -> Decoder b -> Decoder c -> Decoder (a, b, c)
elmStreetDecodeTriple decA decB decC = D.map3 (\a b c -> (a,b,c)) (D.index 0 decA) (D.index 1 decB) (D.index 2 decC)

elmStreetDecodeNonEmpty : Decoder a -> Decoder (a, List a)
elmStreetDecodeNonEmpty decA = D.list decA |> D.andThen (\xs -> case xs of
                                                 h::t -> D.succeed (h, t)
                                                 _    -> D.fail "Expecting at least ONE ELEMENT array")

