module Api.Repository exposing (Repository, fetchByUsername, repositoryDecoder)

import Api.Decoder exposing (nullable, optional, required)
import Api.Fetch as Api
import Http
import Json.Decode as D
import Platform exposing (Task)
import Url exposing (Protocol(..))



-- TYPE


type alias Repository =
    { id : Int
    , name : String
    , full_name : String
    , html_url : String
    , description : Maybe String
    , fork : Bool
    , language : Maybe String
    , forks : Int
    , watchers : Int
    , topics : List String
    , created_at : String
    , svn_url : String
    , readme : Maybe String
    }



-- DECODERS


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
        |> required "created_at" D.string
        |> required "svn_url" D.string
        |> optional "readme" D.string



-- HTTP REQUEST


fetchByUsername : String -> Task Http.Error (List Repository)
fetchByUsername username =
    Api.fetch
        { endpoint = Api.buildUrl [ "users", username, "repos" ] []
        , decoder = D.list repositoryDecoder
        , body = Nothing
        , method = Api.methods.get
        }
