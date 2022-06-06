module Page.Search exposing (Model, Msg(..), init, view)

import Api.Profile exposing (Profile)
import Browser exposing (UrlRequest(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Layout



-- MODEL


type alias Model =
    { profiles : Maybe (List Profile)
    , layout : Layout.Model
    }


type Msg
    = Noop



-- INIT


init : Layout.Model -> ( Model, Cmd msg )
init layout =
    ( { profiles = Nothing, layout = layout }, Cmd.none )



--VIEW


placeholder : Int -> Html msg
placeholder _ =
    div [ class "placeholder-glow mb-4" ]
        [ h3 [ class "" ] [ span [ class "placeholder col-4 d-block" ] [] ]
        , p [ class "" ]
            [ span [ class "placeholder col-9 d-block mb-2" ] []
            , span [ class "placeholder col-7 d-block" ] []
            ]
        ]


view : () -> Html msg
view _ =
    div [ class "row pt-4" ]
        [ div [ class "col-md-2" ]
            [ div [ class "list-group mt-5" ]
                [ button [ class "list-group-item list-group-item-action active" ] [ text "Repositories" ]
                , button [ class "list-group-item list-group-item-action" ] [ text "Profiles" ]
                , button [ class "list-group-item list-group-item-action" ] [ text "Wikis" ]
                , button [ class "list-group-item list-group-item-action" ] [ text "Discussions" ]
                , button [ class "list-group-item list-group-item-action" ] [ text "Issues" ]
                ]
            ]
        , div [ class "col-md-8" ]
            [ h2 [ class "lead border-1 border-bottom fs-3 pb-2 border-dark mb-4" ] [ text "Showing 2,816,887 available repository results" ]
            , div []
                (List.range 1 5
                    |> List.map placeholder
                )
            ]
        , div [ class "col-md-2" ]
            [ h6 [ class "mt-5" ] [ text "Quick search:" ]
            , div [ class "list-group" ]
                [ button [ class "list-group-item list-group-item-action list-group-item-light active" ] [ text "Most populars" ]
                ]
            , h6 [ class "mt-4" ] [ text "Sort options:" ]
            , div [ class "list-group" ]
                [ button [ class "list-group-item list-group-item-action list-group-item-light" ] [ text "Best match" ]
                , button [ class "list-group-item list-group-item-action list-group-item-light" ] [ text "Most starts" ]
                , button [ class "list-group-item list-group-item-action list-group-item-light" ] [ text "Most forks" ]
                , button [ class "list-group-item list-group-item-action list-group-item-light" ] [ text "Recently updated" ]
                ]
            ]
        ]
