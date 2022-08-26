module Api.Topic exposing (..)

import Api.Decoder exposing (nullable, required)
import Api.Fetch as Api
import Html exposing (..)
import Html.Attributes exposing (class, href, target)
import Json.Decode as D exposing (Decoder)



-- TYPES


type alias Topic =
    { name : String
    , display_name : String
    , short_description : String
    , description : String
    , created_by : Maybe String
    , released : Maybe String
    , created_at : String
    , updated_at : String
    , featured : Bool
    , curated : Bool
    }



-- DECODERS


topicDecoder : Decoder Topic
topicDecoder =
    D.succeed Topic
        |> required "name" D.string
        |> required "display_name" D.string
        |> required "short_description" D.string
        |> required "description" D.string
        |> nullable "created_by" D.string
        |> nullable "released" D.string
        |> required "created_at" D.string
        |> required "updated_at" D.string
        |> required "featured" D.bool
        |> required "curated" D.bool



-- HTML HELPERS


renderTag : Bool -> String -> String -> Html msg
renderTag boolean value classNames =
    if boolean then
        span [ class "me-4" ] [ i [ class classNames ] [], text value ]

    else
        text ""


renderTopicCard : Topic -> Html msg
renderTopicCard topic =
    div [ class "col-lg-6 col-md-12" ]
        [ div [ class "card mb-4" ]
            [ div [ class "card-body" ]
                [ div []
                    [ h5
                        [ class "card-title text-secondary" ]
                        [ a [ href <| "https://github.com/topics/" ++ topic.name, target "_blank" ]
                            [ text topic.display_name ]
                        ]
                    , p [] [ text topic.short_description ]
                    , small []
                        [ renderTag topic.featured "featured" "bi bi-star-fill text-primary me-2"
                        , renderTag topic.curated "curated" "bi bi-check-square-fill text-success me-2"
                        ]
                    ]
                ]
            ]
        ]
