module Api.Profile exposing (Profile, ProfileMini, addRepos, fetchByUsername, searchDecoder)

import Api.Decoder exposing (nullable, optional, required)
import Api.Fetch as Api
import Api.Repository as Repo exposing (Repository)
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


type alias ProfileMini =
    { login : String
    , avatar_url : String
    , url : String
    , id : Int
    }



-- DECODERS


searchDecoder : D.Decoder ProfileMini
searchDecoder =
    D.succeed ProfileMini
        |> required "login" D.string
        |> required "avatar_url" D.string
        |> required "url" D.string
        |> required "id" D.int


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
        |> optional "readme" (D.list Repo.repositoryDecoder)



-- HELPERS
{- Adds repos to profile. -}


addRepos : Profile -> List Repository -> Profile
addRepos profile repos =
    { profile | repos = Just repos }



-- HTTP REQUEST


attachProfileRepos : Profile -> Task Http.Error Profile
attachProfileRepos profile =
    Repo.fetchByUsername profile.login
        |> Task.map (addRepos profile)


fetchByUsername : String -> Task Http.Error Profile
fetchByUsername username =
    Api.fetch
        { endpoint = Api.buildUrl [ "users", username ] []
        , decoder = profileDecoder
        , body = Nothing
        , method = Api.methods.get
        }
        |> Task.andThen attachProfileRepos
