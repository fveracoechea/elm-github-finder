module Page.Search exposing (Model, Msg(..), init, update, view)

import Api.Profile exposing (ProfileMini)
import Api.Repository as Repo exposing (Repository)
import Api.Search exposing (..)
import Api.Topic as Repo exposing (Topic, renderTopicCard, topicDecoder)
import Browser exposing (UrlRequest(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Layout
import List.Extra
import Process
import Routing
import Task exposing (Task)
import Url.Builder as UrlBuilder
import Url.Parser exposing (query)



-- MODEL


type SearchStatus
    = Loading
    | GotProfileData (SearchResults ProfileMini)
    | GotRepositoryData (SearchResults Repository)
    | GotTopicData (SearchResults Topic)
    | Failed Http.Error


type alias Model =
    { layout : Layout.Model
    , results : SearchStatus
    , activeCategory : SearchCategory
    }


type Msg
    = Noop
    | GotProfileSearch (Result Http.Error (SearchResults ProfileMini))
    | GotRepositorySearch (Result Http.Error (SearchResults Repository))
    | GotTopicSearch (Result Http.Error (SearchResults Topic))
    | GotNewSortBy Label
    | GotFilterSearch Label



-- INIT


setActiveSortByLabel : Maybe Label -> SearchCategory -> SearchCategory
setActiveSortByLabel maybeLabel category =
    case maybeLabel of
        Just sortByLabel ->
            mapSearchCategory
                (\categoryLabel (SortBy sort) filters ->
                    ( categoryLabel, SortBy (onOptionSelected sort sortByLabel), filters )
                )
                category

        Nothing ->
            category


setActiveFilterLabel : Maybe Label -> SearchCategory -> SearchCategory
setActiveFilterLabel maybeLabel category =
    case maybeLabel of
        Just filterLabel ->
            mapSearchCategory
                (\categoryLabel sort (Filters heading filters) ->
                    ( categoryLabel, sort, Filters heading (onOptionSelected filters filterLabel) )
                )
                category

        Nothing ->
            category


getActiveSortBy : SearchCategory -> Maybe Options
getActiveSortBy (SearchCategory _ (SortBy options) _) =
    options
        |> List.Extra.find
            (\(Options _ _ (IsActive isActive)) ->
                isActive
            )


getActiveFilterLabel : SearchCategory -> Maybe Label
getActiveFilterLabel (SearchCategory _ _ (Filters _ options)) =
    options
        |> List.Extra.find
            (\(Options _ _ (IsActive isActive)) ->
                isActive
            )
        |> Maybe.map getLabelFromOptions


init : Layout.Model -> ( Model, Cmd Msg )
init layout =
    getNewCategorySearch
        { results = Loading
        , layout = layout
        , activeCategory =
            case layout.searchEntity of
                Layout.Profile ->
                    profiles

                Layout.Repository ->
                    repositories

                Layout.Topic ->
                    topics
        }
        Nothing
        Nothing



-- UPDATE


getSortByParams : Maybe Label -> SearchCategory -> List UrlBuilder.QueryParameter
getSortByParams maybeLabel category =
    let
        (SearchCategory _ (SortBy options) _) =
            category

        getParams sortByOptions =
            sortByOptions
                |> getParamsFromOptions
                |> List.map paramsToUrlQuery
    in
    case maybeLabel of
        Just (Label selectedLabel) ->
            let
                maybeOptions =
                    List.Extra.find
                        (\(Options (Label label) _ _) ->
                            selectedLabel == label
                        )
                        options
            in
            case maybeOptions of
                Nothing ->
                    getParams defaultSortBy

                Just (Options _ params _) ->
                    List.map paramsToUrlQuery params

        Nothing ->
            category
                |> getActiveSortBy
                |> Maybe.withDefault defaultSortBy
                |> getParams


getActiveFilterQuery : Maybe Label -> SearchCategory -> String
getActiveFilterQuery maybeLabel (SearchCategory _ _ (Filters _ filters)) =
    let
        reduce params =
            List.foldl reduceFiltersToString "" params
    in
    case maybeLabel of
        Just (Label filterLabel) ->
            let
                maybeOptions =
                    List.Extra.find
                        (\(Options (Label label) _ _) ->
                            filterLabel == label
                        )
                        filters
            in
            case maybeOptions of
                Nothing ->
                    ""

                Just (Options _ params _) ->
                    reduce params

        Nothing ->
            let
                maybeFilter =
                    List.Extra.find
                        (\(Options _ _ (IsActive isActive)) ->
                            isActive
                        )
                        filters
            in
            case maybeFilter of
                Just (Options _ params _) ->
                    reduce params

                Nothing ->
                    ""


fetchSearch : (Result Http.Error (SearchResults a) -> Msg) -> Task Http.Error (SearchResults a) -> Cmd Msg
fetchSearch message task =
    Process.sleep 800
        |> Task.andThen (\_ -> task)
        |> Task.attempt message


getNewCategorySearch : Model -> Maybe Label -> Maybe Label -> ( Model, Cmd Msg )
getNewCategorySearch model maybeSortByLabel maybeFilterLabel =
    let
        (SearchCategory categoryLabel _ _) =
            model.activeCategory

        filter =
            getActiveFilterQuery maybeFilterLabel model.activeCategory

        params : List UrlBuilder.QueryParameter
        params =
            getSortByParams maybeSortByLabel model.activeCategory
                |> List.append [ UrlBuilder.int "per_page" 60 ]

        updateActiveStates category =
            category
                |> setActiveFilterLabel maybeFilterLabel
                |> setActiveSortByLabel maybeSortByLabel
    in
    case categoryLabel of
        Repositories ->
            let
                query =
                    if not (String.isEmpty model.layout.query) then
                        model.layout.query

                    else
                        "stars:>=1000"
            in
            ( { model | activeCategory = updateActiveStates repositories, results = Loading }
            , List.append [ UrlBuilder.string "q" (query ++ filter) ] params
                |> search Repo.repositoryDecoder "repositories"
                |> fetchSearch GotRepositorySearch
            )

        Profiles ->
            let
                query =
                    if not (String.isEmpty model.layout.query) then
                        model.layout.query

                    else
                        "followers:>=1000"
            in
            ( { model | activeCategory = updateActiveStates profiles, results = Loading }
            , List.append [ UrlBuilder.string "q" (query ++ filter) ] params
                |> search Api.Profile.searchDecoder "users"
                |> fetchSearch GotProfileSearch
            )

        Topics ->
            let
                query =
                    if not (String.isEmpty model.layout.query) then
                        model.layout.query

                    else
                        "is:featured"
            in
            ( { model | activeCategory = updateActiveStates topics, results = Loading }
            , List.append [ UrlBuilder.string "q" (query ++ filter) ] params
                |> search topicDecoder "topics"
                |> fetchSearch GotTopicSearch
            )


onOptionSelected : List Options -> Label -> List Options
onOptionSelected options (Label label) =
    options
        |> List.map
            (\(Options (Label itemLable) query _) ->
                if label == itemLable then
                    Options (Label itemLable) query (IsActive True)

                else
                    Options (Label itemLable) query (IsActive False)
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRepositorySearch result ->
            let
                results =
                    case result of
                        Err reason ->
                            Failed reason

                        Ok data ->
                            GotRepositoryData data
            in
            ( { model | results = results }, Cmd.none )

        GotProfileSearch result ->
            let
                results =
                    case result of
                        Err reason ->
                            Failed reason

                        Ok data ->
                            GotProfileData data
            in
            ( { model | results = results }, Cmd.none )

        GotTopicSearch result ->
            let
                results =
                    case result of
                        Err reason ->
                            Failed reason

                        Ok data ->
                            GotTopicData data
            in
            ( { model | results = results }, Cmd.none )

        GotFilterSearch filterLabel ->
            let
                activeSortByLabel =
                    model.activeCategory
                        |> getActiveSortBy
                        |> Maybe.map getLabelFromOptions
            in
            getNewCategorySearch model activeSortByLabel (Just filterLabel)

        GotNewSortBy sortByLabel ->
            getNewCategorySearch model (Just sortByLabel) (getActiveFilterLabel model.activeCategory)

        _ ->
            ( model, Cmd.none )



--VIEW


renderProfile : ProfileMini -> Html Msg
renderProfile p =
    div [ class "col-lg-4 col-md-6 mb-4" ]
        [ div [ class "card " ]
            [ div [ class "card-img-top profile-card-image" ]
                [ a [ Routing.href (Routing.ProfileDetail p.login) ]
                    [ img [ src p.avatar_url, alt p.login ] [] ]
                ]
            , div [ class "card-body d-flex justify-content-between" ]
                [ h5 [ class "card-title text-dark" ] [ text p.login ]
                , button [ class "btn" ] [ i [ class "bi bi-heart" ] [] ]
                ]
            ]
        ]


placeholder : Int -> Html Msg
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

        GotProfileData data ->
            List.map renderProfile data.items

        GotRepositoryData results ->
            List.map (Repo.renderCard 12 True) results.items

        GotTopicData results ->
            List.map renderTopicCard results.items

        _ ->
            []


getCategoryBtn : String -> CategoryLabel -> Bool -> Html Msg
getCategoryBtn query label isActive =
    let
        className =
            "list-group-item list-group-item-action list-group-item-primary"

        classes =
            if isActive then
                String.append className " active"

            else
                className

        q =
            if not (String.isEmpty query) then
                Just query

            else
                Nothing
    in
    a
        [ class classes
        , href (getCategoryUrl q label)
        ]
        [ text (getCategoryName label) ]


getOptionBtn : (String -> Msg) -> Options -> Html Msg
getOptionBtn getMsg (Options (Label label) _ (IsActive isActive)) =
    let
        className =
            "list-group-item list-group-item-action list-group-item-light"

        classes =
            if isActive then
                String.append className " active"

            else
                className
    in
    button [ class classes, onClick (getMsg label) ] [ text label ]


renderSortByOptions : Model -> List (Html Msg)
renderSortByOptions model =
    let
        (SearchCategory _ (SortBy options) _) =
            model.activeCategory

        getMsg : String -> Msg
        getMsg label =
            GotNewSortBy (Label label)
    in
    List.map (getOptionBtn getMsg) options


rederFilters : Model -> List (Html Msg)
rederFilters model =
    let
        (SearchCategory _ _ (Filters _ options)) =
            model.activeCategory

        getMsg : String -> Msg
        getMsg label =
            GotFilterSearch (Label label)
    in
    List.map (getOptionBtn getMsg) options


getFilterHeading : Model -> String
getFilterHeading model =
    let
        (SearchCategory _ _ (Filters heading _)) =
            model.activeCategory
    in
    heading ++ ": "


renderSidebarBtn : Model -> List (Html Msg)
renderSidebarBtn model =
    let
        (SearchCategory currentLabel _ _) =
            model.activeCategory
    in
    availableOptions
        |> List.map
            (\(SearchCategory label _ _) ->
                getCategoryBtn model.layout.query label (currentLabel == label)
            )


renderHeading : Model -> List (Html Msg)
renderHeading model =
    case model.results of
        GotProfileData _ ->
            [ text "Profiles:"
            ]

        GotRepositoryData _ ->
            [ text "Repositories:"
            ]

        GotTopicData _ ->
            [ text "Topics:"
            ]

        _ ->
            [ text "Loading... " ]


renderSortBySidebar : Model -> List (Html Msg)
renderSortBySidebar model =
    let
        shared =
            [ h6 [ class "mt-4" ] [ text "Sort options:" ]
            , div [ class "list-group" ] (renderSortByOptions model)
            ]
    in
    case model.activeCategory of
        SearchCategory Profiles _ _ ->
            shared

        SearchCategory Repositories _ _ ->
            shared

        _ ->
            []


view : Model -> Html Msg
view model =
    div [ class "row pt-4" ]
        [ div [ class "col-lg-3 col-md-4" ]
            [ div [ class "sticky-sm-top mb-4", style "top" "84px" ]
                (List.append
                    [ div [ class "list-group" ] (renderSidebarBtn model)
                    , h6 [ class "mt-4" ] [ text (getFilterHeading model) ]
                    , div [ class "list-group" ] (rederFilters model)
                    ]
                    (renderSortBySidebar model)
                )
            ]
        , div [ class "col-lg-9 col-md-8" ]
            [ h2
                [ class "lead border-1 border-bottom fs-3 pb-1 border-dark" ]
                (renderHeading model)
            , div [ class "row pt-2" ] (renderResults model)
            ]
        ]
