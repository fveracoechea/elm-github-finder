module Api exposing (..)

import Http
import Json.Decode as D exposing (Decoder)
import Types exposing (..)
import Url exposing (Protocol(..))


getUserEndpoint : String -> String
getUserEndpoint username =
    "https://api.github.com/users/" ++ username


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    D.map2 (|>)


requiredField : String -> Decoder a -> Decoder (a -> b) -> Decoder b
requiredField fieldName decoder =
    andMap (D.field fieldName decoder)


optionalField : String -> Decoder a -> Decoder (Maybe a -> b) -> Decoder b
optionalField fieldName decoder =
    andMap (D.field fieldName (D.nullable decoder))


requiredAt : List String -> Decoder a -> Decoder (a -> b) -> Decoder b
requiredAt path decoder =
    andMap (D.at path decoder)


profileDecoder : D.Decoder Profile
profileDecoder =
    D.succeed Profile
        |> requiredField "id" D.int
        |> requiredField "login" D.string
        |> optionalField "name" D.string
        |> requiredField "avatar_url" D.string
        |> requiredField "html_url" D.string
        |> optionalField "bio" D.string
        |> requiredField "location" D.string
        |> optionalField "email" D.string
        |> requiredField "public_repos" D.int
        |> optionalField "blog" D.string
        |> requiredField "followers" D.int
        |> requiredField "following" D.int


fetchUserInfo : String -> Cmd Msg
fetchUserInfo query =
    Http.get
        { url = getUserEndpoint query
        , expect = Http.expectJson GotProfile profileDecoder
        }
