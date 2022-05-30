port module Main exposing (main)

import Api
import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Navigation exposing (Key)
import Html exposing (..)
import Html.Attributes exposing (class)
import Json.Decode as D
import Layout exposing (contentView, headerView)
import Types exposing (..)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>))



-- MODEL


type alias Flags =
    { storedProfile : Maybe String
    }



-- ROUTE
-- type Route
--     = Query String
--     | NotFound
-- routeParser : Parser.Parser (Route -> a) a
-- routeParser =
--     Parser.oneOf
--         [ Parser.map Query (Parser.s "query" </> Parser.string)
--         ]
-- INIT


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags _ key =
    let
        -- parserdUrl =
        --     Maybe.withDefault NotFound (Parser.parse routeParser url)
        emptyModel =
            { navKey = key
            , query = ""
            , profile = Unknown
            }

        maybeModel =
            flags.storedProfile
                |> Maybe.map (D.decodeString Api.profileDecoder)
                |> Maybe.map
                    (\result ->
                        let
                            _ =
                                Debug.log "stored profile" result
                        in
                        case result of
                            Ok data ->
                                { navKey = key
                                , query = ""
                                , profile = Fullfilled data
                                }

                            Err _ ->
                                emptyModel
                    )

        maybeCmd =
            maybeModel
                |> Maybe.map
                    (\newModel ->
                        case newModel.profile of
                            Fullfilled data ->
                                Api.fetchRepos data.login

                            _ ->
                                Cmd.none
                    )
    in
    ( Maybe.withDefault emptyModel maybeModel, Maybe.withDefault Cmd.none maybeCmd )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnSearch ->
            let
                command =
                    if String.isEmpty model.query || String.length model.query < 3 then
                        Cmd.none

                    else
                        Api.fetchProfile model.query
            in
            ( model, command )

        SetQuery newQuery ->
            ( { model | query = newQuery }, Cmd.none )

        SearchProfile query ->
            ( model, Api.fetchProfile query )

        SendUserToExternalUrl url ->
            ( model, Navigation.load url )

        GotRepositories result ->
            let
                _ =
                    Debug.log "GotRepositories" result
            in
            ( model, Cmd.none )

        GotProfile result ->
            case result of
                Ok data ->
                    ( { model | profile = Fullfilled data }
                    , Cmd.batch
                        [ sendProfileToStorage data
                        , Api.fetchRepos data.login
                        ]
                    )

                Err _ ->
                    ( model, Cmd.none )

        Noop ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Github Finder"
    , body =
        [ headerView model
        , contentView model
        ]
    }



-- onUrlRequest


onUrlRequest : UrlRequest -> Msg
onUrlRequest urlRequest =
    case urlRequest of
        External externalUrl ->
            SendUserToExternalUrl externalUrl

        Internal _ ->
            Noop



-- onUrlChange


onUrlChange : Url -> Msg
onUrlChange _ =
    Noop



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }



-- PORTS


port sendProfileToStorage : Profile -> Cmd msg
