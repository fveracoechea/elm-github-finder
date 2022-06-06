module Api.Fetch exposing (Endpoint(..), Request, fetch, methods)

import Http
import Json.Decode exposing (Decoder)
import Task exposing (Task)


type Endpoint
    = Endpoint String


getUrl : Endpoint -> String
getUrl (Endpoint path) =
    "https://api.github.com" ++ path


methods : { get : String, post : String }
methods =
    { get = "GET"
    , post = "POST"
    }


type alias Request a =
    { endpoint : Endpoint
    , decoder : Decoder a
    , method : String
    , headers : Maybe (List Http.Header)
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
        , headers = Maybe.withDefault [] config.headers
        , url = getUrl config.endpoint
        , body = Maybe.withDefault Http.emptyBody config.body
        , resolver =
            config.decoder
                |> handleJsonResponse
                |> Http.stringResolver
        , timeout = Nothing
        }
