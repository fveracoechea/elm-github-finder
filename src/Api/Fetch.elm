module Api.Fetch exposing (Endpoint(..), Request, buildUrl, fetch, methods)

import Http
import Json.Decode exposing (Decoder)
import Task exposing (Task)
import Url.Builder as Builder


type Endpoint
    = Endpoint String


buildUrl : List String -> List Builder.QueryParameter -> Endpoint
buildUrl pathSegments parameters =
    Builder.crossOrigin "https://api.github.com" pathSegments parameters
        |> Endpoint


getUrl : Endpoint -> String
getUrl (Endpoint url) =
    url


methods : { get : String, post : String }
methods =
    { get = "GET"
    , post = "POST"
    }


headers : List Http.Header
headers =
    [ Http.header "Accept" "application/vnd.github.v3+json" ]


type alias Request a =
    { endpoint : Endpoint
    , decoder : Decoder a
    , method : String
    , body : Maybe Http.Body
    }


handleJsonResponse : Decoder a -> Http.Response String -> Result Http.Error a
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.GoodStatus_ _ body ->
            case Json.Decode.decodeString decoder body of
                Err _ ->
                    Err (Http.BadBody body)

                Ok result ->
                    Ok result


fetch : Request a -> Task Http.Error a
fetch config =
    Http.task
        { method = config.method
        , headers = headers
        , url = getUrl config.endpoint
        , body = Maybe.withDefault Http.emptyBody config.body
        , resolver =
            config.decoder
                |> handleJsonResponse
                |> Http.stringResolver
        , timeout = Nothing
        }
