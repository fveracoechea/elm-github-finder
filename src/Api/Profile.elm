module Api.Profile exposing (Profile, fetchProfile)

import Api.Decoder exposing (nullable, optional, required)
import Api.Fetch as Api
import Api.Repository exposing (Repository, repositoryDecoder)
import Http
import Json.Decode as D
import Task exposing (Task)



-- TYPES


type alias Profile =
    { id : Int
    , login : String
    , name : Maybe String
    , avatar_url : String
    , html_url : String
    , bio : Maybe String
    , location : Maybe String
    , email : Maybe String
    , blog : Maybe String
    , followers : Int
    , following : Int
    , public_repos : Int
    , public_gists : Int
    , repos : Maybe (List Repository)
    }



-- DECODERS


profileDecoder : D.Decoder Profile
profileDecoder =
    D.succeed Profile
        |> required "id" D.int
        |> required "login" D.string
        |> nullable "name" D.string
        |> required "avatar_url" D.string
        |> required "html_url" D.string
        |> nullable "bio" D.string
        |> nullable "location" D.string
        |> nullable "email" D.string
        |> nullable "blog" D.string
        |> required "followers" D.int
        |> required "following" D.int
        |> required "public_repos" D.int
        |> required "public_gists" D.int
        |> optional "readme" (D.list repositoryDecoder)



-- HELPERS
{- Adds repos to profile. -}


addRepos : Profile -> List Repository -> Profile
addRepos profile repos =
    { profile | repos = Just repos }



-- HTTP REQUEST


attachProfileRepos : Profile -> Task Http.Error Profile
attachProfileRepos profile =
    Api.fetch
        { endpoint = Api.Endpoint ("/users/" ++ profile.login ++ "/repos")
        , decoder = D.list repositoryDecoder
        , body = Nothing
        , headers = Nothing
        , method = Api.methods.get
        }
        |> Task.map (addRepos profile)


fetchProfile : String -> Task Http.Error Profile
fetchProfile username =
    Api.fetch
        { endpoint = Api.Endpoint ("/users/" ++ username)
        , decoder = profileDecoder
        , body = Nothing
        , headers = Nothing
        , method = Api.methods.get
        }
        |> Task.andThen attachProfileRepos
