module Api exposing (..)

import Http
import Json.Decode as D exposing (Decoder)
import Types exposing (..)
import Url exposing (Protocol(..))



-- HELPERS


getUserEndpoint : String -> String
getUserEndpoint username =
    "https://api.github.com/users/" ++ username


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    D.map2 (|>)



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


profileDecoder : D.Decoder Profile
profileDecoder =
    D.succeed Profile
        |> required "id" D.int
        |> required "login" D.string
        |> nullable "name" D.string
        |> required "avatar_url" D.string
        |> required "html_url" D.string
        |> nullable "bio" D.string
        |> required "location" D.string
        |> nullable "email" D.string
        |> required "public_repos" D.int
        |> nullable "blog" D.string
        |> required "followers" D.int
        |> required "following" D.int


repositoryDecoder : D.Decoder Repository
repositoryDecoder =
    D.succeed Repository
        |> required "id" D.int
        |> required "name" D.string
        |> required "full_name" D.string
        |> required "html_url" D.string
        |> nullable "description" D.string
        |> required "fork" D.bool
        |> nullable "language" D.string
        |> required "forks" D.int
        |> required "watchers" D.int
        |> required "topics" (D.list D.string)
        |> optional "readme" D.string


repositoriesDecoder : D.Decoder (List Repository)
repositoriesDecoder =
    D.list repositoryDecoder


fetchRepos : String -> Cmd Msg
fetchRepos username =
    Http.get
        { url = getUserEndpoint username ++ "/repos"
        , expect = Http.expectJson GotRepositories (D.list repositoryDecoder)
        }


fetchProfile : String -> Cmd Msg
fetchProfile query =
    Http.get
        { url = getUserEndpoint query
        , expect = Http.expectJson GotProfile profileDecoder
        }
