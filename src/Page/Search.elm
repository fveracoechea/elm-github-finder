module Page.Search exposing (Model, Msg(..), init, update, view)

import Api.Profile exposing (ProfileMini)
import Api.Repository as Repo exposing (Repository)
import Api.Search exposing (..)
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



-- MODEL


type SearchStatus
    = Loading
    | GotProfileData (SearchResults ProfileMini)
    | GotRepositoryData (SearchResults Repository)
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
    | GotNewCategory CategoryLabel
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
                (\categoryLabel sort (Filters filters) ->
                    ( categoryLabel, sort, Filters (onOptionSelected filters filterLabel) )
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
getActiveFilterLabel (SearchCategory _ _ (Filters options)) =
    options
        |> List.Extra.find
            (\(Options _ _ (IsActive isActive)) ->
                isActive
            )
        |> Maybe.map getLabelFromOptions


init : Layout.Model -> ( Model, Cmd Msg )
init layout =
    let
        defaultCategory =
            profiles

        ( cmd, category ) =
            if not (String.isEmpty layout.query) then
                ( Process.sleep 800
                    |> Task.andThen (\_ -> search Api.Profile.searchDecoder "users" [ UrlBuilder.string "q" layout.query ])
                    |> Task.attempt GotProfileSearch
                , defaultCategory
                )

            else
                ( Process.sleep 800
                    |> Task.andThen (\_ -> searchMostPopularProfiles 1 [])
                    |> Task.attempt GotProfileSearch
                , defaultCategory
                )
    in
    ( { results = Loading, layout = layout, activeCategory = category }, cmd )



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
getActiveFilterQuery maybeLabel (SearchCategory _ _ (Filters filters)) =
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

        query =
            model.layout.query ++ filter

        queryParams =
            List.append [ UrlBuilder.string "q" query ] params

        updateActiveStates category =
            category
                |> setActiveFilterLabel maybeFilterLabel
                |> setActiveSortByLabel maybeSortByLabel
    in
    case categoryLabel of
        Repositories ->
            let
                category =
                    updateActiveStates repositories

                task =
                    if not (String.isEmpty query) then
                        search Repo.repositoryDecoder "repositories" queryParams

                    else
                        searchMostPopularRepositories 1 params
            in
            ( { model | activeCategory = category, results = Loading }
            , fetchSearch GotRepositorySearch task
            )

        _ ->
            let
                category =
                    updateActiveStates profiles

                task =
                    if not (String.isEmpty query) then
                        search Api.Profile.searchDecoder "users" queryParams

                    else
                        searchMostPopularProfiles 1 params
            in
            ( { model | activeCategory = category, results = Loading }
            , fetchSearch GotProfileSearch task
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

        GotNewCategory newCategoryLabel ->
            let
                (SearchCategory currentCategoryLabel (SortBy options) filters) =
                    model.activeCategory
            in
            if currentCategoryLabel == newCategoryLabel then
                ( model, Cmd.none )

            else
                getNewCategorySearch
                    { model | activeCategory = SearchCategory newCategoryLabel (SortBy options) filters }
                    Nothing
                    Nothing

        _ ->
            ( model, Cmd.none )



--VIEW


formatNumber : String -> String
formatNumber integers =
    let
        reversedSplitThousands : String -> List String
        reversedSplitThousands value =
            if String.length value > 3 then
                value
                    |> String.dropRight 3
                    |> reversedSplitThousands
                    |> (::) (String.right 3 value)

            else
                [ value ]
    in
    integers
        |> reversedSplitThousands
        |> List.reverse
        |> String.join ","


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

        _ ->
            []


getCategoryBtn : CategoryLabel -> Bool -> Html Msg
getCategoryBtn label isActive =
    let
        className =
            "list-group-item list-group-item-action list-group-item-primary"

        classes =
            if isActive then
                String.append className " active"

            else
                className
    in
    button
        [ class classes
        , onClick (GotNewCategory label)
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
        (SearchCategory _ _ (Filters options)) =
            model.activeCategory

        getMsg : String -> Msg
        getMsg label =
            GotFilterSearch (Label label)
    in
    List.map (getOptionBtn getMsg) options


renderSidebarBtn : Model -> List (Html Msg)
renderSidebarBtn model =
    let
        (SearchCategory currentLabel _ _) =
            model.activeCategory
    in
    availableOptions
        |> List.map
            (\(SearchCategory label _ _) ->
                getCategoryBtn label (currentLabel == label)
            )


renderHeading : Model -> List (Html Msg)
renderHeading model =
    case model.results of
        GotProfileData data ->
            [ text "Showing "
            , b []
                [ data.total_count
                    |> String.fromInt
                    |> formatNumber
                    |> text
                ]
            , text " profiles:"
            ]

        GotRepositoryData data ->
            [ text "Showing "
            , b []
                [ data.total_count
                    |> String.fromInt
                    |> formatNumber
                    |> text
                ]
            , text " repository results:"
            ]

        _ ->
            [ text "Loading... " ]


view : Model -> Html Msg
view model =
    div [ class "row pt-4" ]
        [ div [ class "col-lg-3 col-md-4" ]
            [ div [ class "sticky-sm-top mb-4", style "top" "20px" ]
                [ div [ class "list-group" ] (renderSidebarBtn model)
                , h6 [ class "mt-4" ] [ text "Languages:" ]
                , div [ class "list-group" ] (rederFilters model)
                , h6 [ class "mt-4" ] [ text "Sort options:" ]
                , div [ class "list-group" ] (renderSortByOptions model)
                ]
            ]
        , div [ class "col-lg-9 col-md-8" ]
            [ h2 [ class "lead border-1 border-bottom fs-3 pb-2 border-dark" ] (renderHeading model)
            , div [ class "sticky-sm-top", style "top" "0", style "background-color" "#f2f2f2", style "padding-top" "20px" ]
                [ div [ class "d-flex justify-content-end" ]
                    [ nav [ attribute "aria-label" "Page navigation example" ]
                        [ ul [ class "pagination" ]
                            [ li [ class "page-item" ]
                                [ a [ attribute "aria-label" "Previous", class "page-link", href "#" ]
                                    [ span [ attribute "aria-hidden" "true" ]
                                        [ i [ class "bi bi-chevron-double-left" ] [] ]
                                    ]
                                ]
                            , li [ class "page-item" ]
                                [ a [ attribute "aria-label" "Previous", class "page-link", href "#" ]
                                    [ span [ attribute "aria-hidden" "true" ]
                                        [ text "1" ]
                                    ]
                                ]
                            , li [ class "page-item disabled" ]
                                [ a [ class "page-link", href "#" ]
                                    [ text "..." ]
                                ]
                            , li [ class "page-item" ]
                                [ a [ class "page-link", href "#" ]
                                    [ text "3" ]
                                ]
                            , li [ class "page-item active" ]
                                [ a [ class "page-link", href "#" ]
                                    [ text "4" ]
                                ]
                            , li [ class "page-item" ]
                                [ a [ class "page-link", href "#" ]
                                    [ text "5" ]
                                ]
                            , li [ class "page-item disabled" ]
                                [ a [ attribute "aria-label" "Next", class "page-link", href "#" ]
                                    [ span [ attribute "aria-hidden" "true" ]
                                        [ text "..." ]
                                    ]
                                ]
                            , li [ class "page-item" ]
                                [ a [ attribute "aria-label" "Next", class "page-link", href "#" ]
                                    [ span [ attribute "aria-hidden" "true" ]
                                        [ text "12" ]
                                    ]
                                ]
                            , li [ class "page-item" ]
                                [ a [ attribute "aria-label" "Next", class "page-link", href "#" ]
                                    [ span [ attribute "aria-hidden" "true" ]
                                        [ i [ class "bi bi-chevron-double-right" ] [] ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            , div [ class "row" ] (renderResults model)
            ]
        ]
