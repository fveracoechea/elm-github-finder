module Api.Topic exposing (..)

import Api.Decoder exposing (nullable, required)
import Api.Fetch as Api
import Html exposing (..)
import Html.Attributes exposing (class)
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


renderTag : Bool -> String -> Html msg
renderTag boolean value =
    let
        html =
            if boolean then
                text (value ++ " ")

            else
                text ""
    in
    span [] [ html ]


renderTopicCard : Topic -> Html msg
renderTopicCard topic =
    div [ class "col-lg-6 col-md-12" ]
        [ div [ class "card mb-4" ]
            [ div [ class "card-body" ]
                [ div []
                    [ h5
                        [ class "card-title text-secondary" ]
                        [ text topic.display_name ]
                    , p [] [ text topic.short_description ]
                    , small []
                        [ renderTag topic.featured "featured"
                        , renderTag topic.curated "curated"
                        ]
                    ]
                ]
            ]
        ]
