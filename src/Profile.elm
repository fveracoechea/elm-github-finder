module Profile exposing (Profile, Repository, addRepos, profileDecoder, repositoriesDecoder, repositoryDecoder)

import Json.Decode as D exposing (Decoder)



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



-- INTERNALS
{- Pipeline helper for decoders -}


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



-- HELPERS
{- Adds repos to profile. -}


addRepos : Profile -> List Repository -> Profile
addRepos profile repos =
    { profile | repos = Just repos }
