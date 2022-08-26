module Api.Repository exposing (Repository, fetchByUsername, getLanguageBadge, renderCard, repositoryDecoder)

import Api.Dates as ApiDate
import Api.Decoder exposing (nullable, optional, required)
import Api.Fetch as Api
import Html exposing (..)
import Html.Attributes exposing (class, href, style, target, title)
import Http
import Json.Decode as D exposing (Decoder)
import Platform exposing (Task)
import Url exposing (Protocol(..))



-- TYPES


type alias License =
    { name : String
    , key : String
    , spdx_id : String
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
    , license : Maybe License
    }



-- DECODERS


licenseDecoder : Decoder License
licenseDecoder =
    D.succeed License
        |> required "name" D.string
        |> required "key" D.string
        |> required "spdx_id" D.string


repositoryDecoder : Decoder Repository
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
        |> optional "license" licenseDecoder



-- HTTP REQUEST


fetchByUsername : String -> Task Http.Error (List Repository)
fetchByUsername username =
    Api.fetch
        { endpoint = Api.buildUrl [ "users", username, "repos" ] []
        , decoder = D.list repositoryDecoder
        , body = Nothing
        , method = Api.methods.get
        }



-- HELPERS


getLanguageBadge : String -> Html msg
getLanguageBadge language =
    let
        classNames =
            case language of
                "Elm" ->
                    "badge text-bg-primary"

                "JavaScript" ->
                    "badge text-bg-warning"

                "TypeScript" ->
                    "badge text-bg-info"

                "Python" ->
                    "badge text-bg-success"

                "PHP" ->
                    "badge text-bg-danger"

                "Java" ->
                    "badge text-bg-danger"

                "C#" ->
                    "badge text-bg-light"

                _ ->
                    "badge text-bg-secondary"
    in
    div []
        [ span [ class classNames ]
            [ text language ]
        ]


renderList : Repository -> List (Html msg)
renderList repo =
    [ ApiDate.renderDate repo.created_at
    , renderLicense repo.license
    ]
        |> List.map (\html -> li [ class "d-inline me-4" ] [ html ])


renderLicense : Maybe License -> Html msg
renderLicense maybeLicense =
    case maybeLicense of
        Just license ->
            span
                [ title "license", style "font-size" ".8rem" ]
                [ i
                    [ class "bi bi-file-text-fill text-dark me-2", style "font-size" "1.2rem" ]
                    []
                , text license.spdx_id
                ]

        Nothing ->
            text ""


renderCard : Int -> Bool -> Repository -> Html msg
renderCard cols useFullname repo =
    let
        title =
            if useFullname then
                repo.full_name

            else
                repo.name
    in
    div [ class ("col-lg-" ++ String.fromInt cols) ]
        [ div [ class "card mb-4" ]
            [ div [ class "card-body" ]
                [ div [ class "d-flex justify-content-between" ]
                    [ h5
                        [ class "card-title text-secondary" ]
                        [ a [ href repo.html_url, target "_blank" ]
                            [ text title ]
                        ]
                    , repo.language
                        |> Maybe.map getLanguageBadge
                        |> Maybe.withDefault (text "")
                    ]
                , repo.description
                    |> Maybe.map
                        (\description ->
                            p [ class "card-text text-truncate" ]
                                [ text description ]
                        )
                    |> Maybe.withDefault (p [ style "min-height" "24px" ] [ text " " ])
                , ul [ class "p-0 m-0" ] (renderList repo)
                ]
            ]
        ]
