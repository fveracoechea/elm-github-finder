module Api.Search exposing (..)

import Api.Decoder exposing (required)
import Api.Fetch as Api
import Api.Profile exposing (..)
import Api.Repository exposing (Repository, repositoryDecoder)
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
    | Issues
    | Discussions
    | Wikis


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
    = Filters (List Options)


type SearchCategory
    = SearchCategory CategoryLabel SortBy Filters


defaultSortBy : Options
defaultSortBy =
    Options (Label "Best match") [] (IsActive True)


languages : Filters
languages =
    Filters
        [ Options (Label "Elm") [ Query "language:elm" ] (IsActive False)
        , Options (Label "JavaScript") [ Query "language:javascript" ] (IsActive False)
        , Options (Label "TypeScript") [ Query "language:typescript" ] (IsActive False)
        , Options (Label "Python") [ Query "language:python" ] (IsActive False)
        , Options (Label "PHP") [ Query "language:php" ] (IsActive False)
        , Options (Label "Java") [ Query "language:java" ] (IsActive False)
        , Options (Label "C#") [ Query "language:c#" ] (IsActive False)
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


wikis : SearchCategory
wikis =
    SearchCategory
        Wikis
        (SortBy
            [ defaultSortBy
            ]
        )
        (Filters [])


availableOptions : List SearchCategory
availableOptions =
    [ profiles, repositories, wikis ]



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


searchMostPopularProfiles : Int -> List UrlBuilder.QueryParameter -> Task Http.Error (SearchResults ProfileMini)
searchMostPopularProfiles page params =
    let
        pageQuery =
            [ UrlBuilder.int "per_page" 30
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


searchMostPopularRepositories : Int -> List UrlBuilder.QueryParameter -> Task Http.Error (SearchResults Repository)
searchMostPopularRepositories page params =
    let
        pageQuery =
            [ UrlBuilder.int "per_page" 30
            , UrlBuilder.int "page" page
            , UrlBuilder.string "q" "stars:>=1000"
            ]

        searchQuery =
            if List.length params > 0 then
                params

            else
                [ UrlBuilder.string "sort" "stars"
                , UrlBuilder.string "order" "desc"
                ]
    in
    List.append searchQuery pageQuery
        |> search repositoryDecoder "repositories"



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

        Issues ->
            "Issues"

        Discussions ->
            "Discussions"

        Wikis ->
            "Wikis"


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
