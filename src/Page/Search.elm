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
    | GotNewQuickSearch Label



-- INIT


setActiveOptionByLabel : Label -> SearchCategory -> SearchCategory
setActiveOptionByLabel selectedLabel category =
    mapSearchCategory
        (\label (SortBy sort) quick ->
            ( label, SortBy (onOptionSelected sort selectedLabel), quick )
        )
        category


init : Layout.Model -> ( Model, Cmd Msg )
init layout =
    let
        ( cmd, category ) =
            if not (String.isEmpty layout.query) then
                ( Process.sleep 800
                    |> Task.andThen (\_ -> search Api.Profile.searchDecoder "users" [ UrlBuilder.string "q" layout.query ])
                    |> Task.attempt GotProfileSearch
                , setActiveOptionByLabel (Label "Best match") profiles
                )

            else
                ( Process.sleep 800
                    |> Task.andThen (\_ -> searchMostPopularProfiles 1 [])
                    |> Task.attempt GotProfileSearch
                , setActiveOptionByLabel (Label "") profiles
                )
    in
    ( { results = Loading, layout = layout, activeCategory = category }, cmd )



-- UPDATE


fetchSearch : (Result Http.Error (SearchResults a) -> Msg) -> Task Http.Error (SearchResults a) -> Cmd Msg
fetchSearch message task =
    Process.sleep 800
        |> Task.andThen (\_ -> task)
        |> Task.attempt message


getNewCategorySearch : Model -> CategoryLabel -> Label -> List UrlBuilder.QueryParameter -> ( Model, Cmd Msg )
getNewCategorySearch model label optionLabel params =
    let
        _ =
            Debug.log "params" params

        query =
            List.append [ UrlBuilder.string "q" model.layout.query ] params
    in
    case label of
        Repositories ->
            let
                ( category, searchTask ) =
                    if not (String.isEmpty model.layout.query) then
                        ( setActiveOptionByLabel optionLabel repositories
                        , search Repo.repositoryDecoder "repositories" query
                        )

                    else
                        ( repositories, searchMostPopularRepositories 1 params )
            in
            ( { model | activeCategory = category, results = Loading }
            , fetchSearch GotRepositorySearch searchTask
            )

        _ ->
            let
                ( category, searchTask ) =
                    if not (String.isEmpty model.layout.query) then
                        ( setActiveOptionByLabel optionLabel profiles
                        , search Api.Profile.searchDecoder "users" query
                        )

                    else
                        ( profiles, searchMostPopularProfiles 1 params )
            in
            ( { model | activeCategory = category, results = Loading }
            , fetchSearch GotProfileSearch searchTask
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


getQueryOptions : Label -> List Options -> List UrlBuilder.QueryParameter
getQueryOptions (Label label) options =
    let
        _ =
            Debug.log "new sort by" label

        option =
            options
                |> List.Extra.find
                    (\(Options (Label itemLabel) _ _) ->
                        label == itemLabel
                    )
    in
    case option of
        Nothing ->
            []

        Just (Options _ params _) ->
            List.map paramsToUrlQuery params


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

        GotNewQuickSearch selectedLabel ->
            let
                activeCategory =
                    mapSearchCategory
                        (\label sortBy (Filters options) ->
                            ( label, sortBy, Filters (onOptionSelected options selectedLabel) )
                        )
                        model.activeCategory
            in
            ( { model | activeCategory = activeCategory }, Cmd.none )

        GotNewSortBy selectedLabel ->
            let
                (SearchCategory cLabel sortBy _) =
                    model.activeCategory

                (SortBy options) =
                    sortBy
            in
            getQueryOptions selectedLabel options
                |> getNewCategorySearch model cLabel selectedLabel

        GotNewCategory newLabel ->
            let
                (SearchCategory cLabel _ _) =
                    model.activeCategory
            in
            if cLabel == newLabel then
                ( model, Cmd.none )

            else
                getNewCategorySearch model newLabel (Label "Best match") []

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
            [ a [ Routing.href (Routing.ProfileDetail p.login) ] [ img [ src p.avatar_url, alt p.login, class "card-img-top" ] [] ]
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
            GotNewQuickSearch (Label label)
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
