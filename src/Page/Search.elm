module Page.Search exposing (Model, Msg(..), init, update, view)

import Api.Profile exposing (Profile, ProfileMini)
import Browser exposing (UrlRequest(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Layout
import Process
import Routing
import Task



-- MODEL


type SearchResults
    = Loading
    | MostPopularProfiles (List ProfileMini)
    | Failed Http.Error


type alias Model =
    { layout : Layout.Model
    , results : SearchResults
    }


type Msg
    = Noop
    | GotMostPopularProfiles (Result Http.Error (List ProfileMini))



-- INIT


init : Layout.Model -> ( Model, Cmd Msg )
init layout =
    let
        cmd =
            Process.sleep 1000
                |> Task.andThen Api.Profile.fetchMostPopulars
                |> Task.attempt GotMostPopularProfiles
    in
    ( { results = Loading, layout = layout }, cmd )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotMostPopularProfiles result ->
            let
                results =
                    case result of
                        Err reason ->
                            Failed reason

                        Ok data ->
                            MostPopularProfiles data
            in
            ( { model | results = results }, Cmd.none )

        Noop ->
            ( model, Cmd.none )



--VIEW


renderProfile : ProfileMini -> Html Msg
renderProfile p =
    div [ class "col-lg-4 col-md-6 mb-4" ]
        [ div [ class "card " ]
            [ a [ Routing.href (Routing.ProfileDetail p.login) ] [ img [ src p.avatar_url, alt p.login, class "card-img-top" ] [] ]
            , div [ class "card-body d-flex justify-content-between" ]
                [ h5 [ class "card-title text-dark" ] [ text p.login ]
                , button [ class "btn" ] [ i [ class "bi bi-heart" ] [] ]
                ]
            ]
        ]


placeholder : Int -> Html msg
placeholder _ =
    div [ class "placeholder-glow mb-4" ]
        [ h3 [ class "" ] [ span [ class "placeholder col-4 d-block" ] [] ]
        , p [ class "" ]
            [ span [ class "placeholder col-9 d-block mb-2" ] []
            , span [ class "placeholder col-7 d-block" ] []
            ]
        ]


renderResults : Model -> List (Html Msg)
renderResults model =
    case model.results of
        Loading ->
            List.range 1 5
                |> List.map placeholder

        MostPopularProfiles profiles ->
            List.map renderProfile profiles

        _ ->
            []


view : Model -> Html Msg
view model =
    div [ class "row pt-4" ]
        [ div [ class "col-lg-3 col-md-4" ]
            [ div [ class "sticky-sm-top mb-4", style "top" "20px" ]
                [ div [ class "list-group mt-5" ]
                    [ button [ class "list-group-item list-group-item-action active" ] [ text "Profiles" ]
                    , button [ class "list-group-item list-group-item-action" ] [ text "Repositories" ]
                    , button [ class "list-group-item list-group-item-action" ] [ text "Issues" ]
                    , button [ class "list-group-item list-group-item-action" ] [ text "Discussions" ]
                    , button [ class "list-group-item list-group-item-action" ] [ text "Wikis" ]
                    ]
                , h6 [ class "mt-4" ] [ text "Quick search:" ]
                , div [ class "list-group" ]
                    [ button [ class "list-group-item list-group-item-action list-group-item-light active" ] [ text "Most populars" ]
                    ]
                , h6 [ class "mt-4" ] [ text "Sort options:" ]
                , div [ class "list-group" ]
                    [ button [ class "list-group-item list-group-item-action list-group-item-light" ] [ text "Best match" ]
                    , button [ class "list-group-item list-group-item-action list-group-item-light" ] [ text "Most followers" ]
                    , button [ class "list-group-item list-group-item-action list-group-item-light" ] [ text "Most repositories" ]
                    , button [ class "list-group-item list-group-item-action list-group-item-light" ] [ text "Most recently joined" ]
                    , button [ class "list-group-item list-group-item-action list-group-item-light" ] [ text "Last recently joined" ]
                    ]
                ]
            ]
        , div [ class "col-lg-9 col-md-8" ]
            [ h2 [ class "lead border-1 border-bottom fs-3 pb-2 border-dark mb-4" ] [ text "Showing 2,816,887 available repository results" ]
            , div [ class "row" ] (renderResults model)
            ]
        ]
