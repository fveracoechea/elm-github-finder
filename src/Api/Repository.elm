module Api.Repository exposing (Repository, repositoriesDecoder, repositoryDecoder)

import Api.Decoder exposing (nullable, optional, required)
import Json.Decode as D


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


repositoriesDecoder : D.Decoder (List Repository)
repositoriesDecoder =
    D.list repositoryDecoder
