module Api.Decoder exposing (..)

import Json.Decode as D exposing (Decoder)



-- INTERNALS
{- Pipeline helper for decoders -}


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    D.map2 (|>)



-- HELPERS
{- Decode a required JSON value into an Elm value. -}


required : String -> Decoder a -> Decoder (a -> b) -> Decoder b
required fieldName decoder =
    andMap (D.field fieldName decoder)



{- Decode a nullable JSON value into an Elm value. -}


nullable : String -> Decoder a -> Decoder (Maybe a -> b) -> Decoder b
nullable fieldName decoder =
    andMap (D.field fieldName (D.nullable decoder))



{- Decode a optional JSON value into an Elm value. -}


optional : String -> Decoder a -> Decoder (Maybe a -> b) -> Decoder b
optional fieldName decoder =
    andMap (D.maybe (D.field fieldName decoder))
