module Api.Search exposing (..)

import Api.Decoder exposing (nullable, optional, required)
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
            [ Options (Label "Best match") [] (IsActive False)
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
            [ Options (Label "Best match") [] (IsActive False)
            , Options (Label "Most stars") [ Sort "stars", Order "desc" ] (IsActive False)
            , Options (Label "Most forks") [ Sort "forks", Order "desc" ] (IsActive False)
            , Options (Label "Most recently updated") [ Sort "updated", Order "desc" ] (IsActive False)
            , Options (Label "Last recently updated") [ Sort "updated", Order "asc" ] (IsActive False)
            ]
        )
        languages


issues : SearchCategory
issues =
    SearchCategory
        Issues
        (SortBy
            [ Options (Label "Best match") [] (IsActive False)
            ]
        )
        (Filters [])


discussions : SearchCategory
discussions =
    SearchCategory
        Discussions
        (SortBy
            [ Options (Label "Best match") [] (IsActive False)
            ]
        )
        (Filters [])


wikis : SearchCategory
wikis =
    SearchCategory
        Wikis
        (SortBy
            [ Options (Label "Best match") [] (IsActive False)
            ]
        )
        (Filters [])


availableOptions : List SearchCategory
availableOptions =
    [ profiles, repositories, issues, discussions, wikis ]



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


searchMostPopularProfiles : Int -> Task Http.Error (SearchResults ProfileMini)
searchMostPopularProfiles page =
    search
        Api.Profile.searchDecoder
        "users"
        [ UrlBuilder.string "q" "followers:>=0"
        , UrlBuilder.string "sort" "followers"
        , UrlBuilder.string "order" "desc"
        , UrlBuilder.int "per_page" 30
        , UrlBuilder.int "page" page
        ]


searchMostPopularRepositories : Int -> Task Http.Error (SearchResults Repository)
searchMostPopularRepositories page =
    search
        repositoryDecoder
        "repositories"
        [ UrlBuilder.string "q" "stars:>=0"
        , UrlBuilder.string "sort" "stars"
        , UrlBuilder.string "order" "desc"
        , UrlBuilder.int "per_page" 30
        , UrlBuilder.int "page" page
        ]



-- HELPERS


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
