module Api.Topic exposing (..)

import Api.Decoder exposing (nullable, optional, required)
import Api.Fetch as Api
import Html exposing (..)
import Html.Attributes exposing (class, style, title)
import Json.Decode as D exposing (Decoder)
import Task exposing (Task)



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


renderTopicCard : Topic -> Html msg
renderTopicCard topic =
    div [ class "col-lg-6 col-md-12" ]
        [ div [ class "card mb-4" ]
            [ div [ class "card-body" ]
                [ div [ class "d-flex justify-content-between" ]
                    [ h5
                        [ class "card-title text-secondary" ]
                        [ text topic.display_name ]
                    ]
                ]
            ]
        ]
