module Api.Search exposing (..)

import Api.Decoder exposing (required)
import Api.Fetch as Api
import Api.Profile exposing (..)
import Api.Topic exposing (..)
import Http
import Json.Decode as D
import Task exposing (Task)
import Url.Builder as UrlBuilder



-- TYPES


type alias SearchResults a =
    { items : List a
    , total_count : Int
    }


type Label
    = Label String


type CategoryLabel
    = Repositories
    | Profiles
    | Topics


type Parameters
    = Query String
    | Sort String
    | Order String
    | Page Int


type IsActive
    = IsActive Bool


type Options
    = Options Label (List Parameters) IsActive


type SortBy
    = SortBy (List Options)


type Filters
    = Filters String (List Options)


type SearchCategory
    = SearchCategory CategoryLabel SortBy Filters


defaultSortBy : Options
defaultSortBy =
    Options (Label "Best match") [] (IsActive True)


languages : Filters
languages =
    Filters
        "Languages"
        [ Options (Label "Elm") [ Query "language:elm" ] (IsActive False)
        , Options (Label "JavaScript") [ Query "language:javascript" ] (IsActive False)
        , Options (Label "TypeScript") [ Query "language:typescript" ] (IsActive False)
        , Options (Label "Python") [ Query "language:python" ] (IsActive False)
        , Options (Label "PHP") [ Query "language:php" ] (IsActive False)
        , Options (Label "Java") [ Query "language:java" ] (IsActive False)
        , Options (Label "C#") [ Query "language:c#" ] (IsActive False)
        ]


qualifiers : Filters
qualifiers =
    Filters
        "Filters"
        [ Options (Label "Featured") [ Query "is:featured" ] (IsActive True)
        , Options (Label "Curated") [ Query "is:curated" ] (IsActive False)
        , Options (Label "More than 5000 repositories") [ Query "repositories:>5000" ] (IsActive False)
        , Options (Label "More than 2500 repositories") [ Query "repositories:>2500" ] (IsActive False)
        , Options (Label "More than 1000 repositories") [ Query "repositories:>1000" ] (IsActive False)
        ]


profiles : SearchCategory
profiles =
    SearchCategory
        Profiles
        (SortBy
            [ defaultSortBy
            , Options (Label "Most followers") [ Sort "followers", Order "desc" ] (IsActive False)
            , Options (Label "Most repositories") [ Sort "repositories", Order "desc" ] (IsActive False)
            , Options (Label "Most recently joined") [ Sort "joined", Order "desc" ] (IsActive False)
            , Options (Label "Last recently joined") [ Sort "joined", Order "asc" ] (IsActive False)
            ]
        )
        languages


repositories : SearchCategory
repositories =
    SearchCategory
        Repositories
        (SortBy
            [ defaultSortBy
            , Options (Label "Most stars") [ Sort "stars", Order "desc" ] (IsActive False)
            , Options (Label "Most forks") [ Sort "forks", Order "desc" ] (IsActive False)
            , Options (Label "Most recently updated") [ Sort "updated", Order "desc" ] (IsActive False)
            , Options (Label "Last recently updated") [ Sort "updated", Order "asc" ] (IsActive False)
            ]
        )
        languages


topics : SearchCategory
topics =
    SearchCategory
        Topics
        (SortBy
            [ Options (Label "Most recently updated") [ Sort "updated", Order "desc" ] (IsActive False)
            , Options (Label "Last recently updated") [ Sort "updated", Order "asc" ] (IsActive False)
            ]
        )
        qualifiers


availableOptions : List SearchCategory
availableOptions =
    [ profiles, repositories, topics ]



-- HTTP REQUESTS


search : D.Decoder a -> String -> List UrlBuilder.QueryParameter -> Task Http.Error (SearchResults a)
search itemDecoder category parameters =
    Api.fetch
        { endpoint = Api.buildUrl [ "search", category ] parameters
        , decoder =
            D.succeed SearchResults
                |> required "items" (D.list itemDecoder)
                |> required "total_count" D.int
        , body = Nothing
        , method = Api.methods.get
        }


searchTopicByName : Int -> List UrlBuilder.QueryParameter -> Task Http.Error (SearchResults Topic)
searchTopicByName perPage params =
    let
        query =
            List.append
                params
                [ UrlBuilder.int "per_page" perPage
                ]
    in
    search Api.Topic.topicDecoder "topics" query


searchMostPopularProfiles : Int -> List UrlBuilder.QueryParameter -> Task Http.Error (SearchResults ProfileMini)
searchMostPopularProfiles page params =
    let
        pageQuery =
            [ UrlBuilder.int "per_page" 60
            , UrlBuilder.int "page" page
            , UrlBuilder.string "q" "followers:>=1000"
            ]

        searchQuery =
            if List.length params > 0 then
                params

            else
                [ UrlBuilder.string "sort" "followers"
                , UrlBuilder.string "order" "desc"
                ]
    in
    List.append searchQuery pageQuery
        |> search Api.Profile.searchDecoder "users"



-- HELPERS


reduceFiltersToString : Parameters -> String -> String
reduceFiltersToString params args =
    case params of
        Query query ->
            String.append args (" " ++ query)

        _ ->
            String.append args ""


getParamsFromOptions : Options -> List Parameters
getParamsFromOptions (Options _ params _) =
    params


getLabelFromOptions : Options -> Label
getLabelFromOptions (Options label _ _) =
    label


paramsToUrlQuery : Parameters -> UrlBuilder.QueryParameter
paramsToUrlQuery param =
    case param of
        Query value ->
            UrlBuilder.string "q" value

        Sort value ->
            UrlBuilder.string "sort" value

        Order value ->
            UrlBuilder.string "order" value

        Page value ->
            UrlBuilder.int "page" value


getCategoryName : CategoryLabel -> String
getCategoryName label =
    case label of
        Profiles ->
            "Profiles"

        Repositories ->
            "Repositories"

        Topics ->
            "Topics"


getCategoryUrl : Maybe String -> CategoryLabel -> String
getCategoryUrl maybeQuery label =
    let
        query =
            case maybeQuery of
                Just q ->
                    [ UrlBuilder.string "query" q ]

                Nothing ->
                    []

        build =
            UrlBuilder.absolute [ "search" ]
    in
    case label of
        Profiles ->
            build (List.append [ UrlBuilder.string "entity" "profile" ] query)

        Repositories ->
            build (List.append [ UrlBuilder.string "entity" "repository" ] query)

        Topics ->
            build (List.append [ UrlBuilder.string "entity" "topic" ] query)


mapSearchCategory :
    (CategoryLabel -> SortBy -> Filters -> ( CategoryLabel, SortBy, Filters ))
    -> SearchCategory
    -> SearchCategory
mapSearchCategory mapFn (SearchCategory label_ sortOptions_ quickSearch_) =
    let
        ( label, options, quickSearch ) =
            mapFn label_ sortOptions_ quickSearch_
    in
    SearchCategory label options quickSearch
