module Page.ProfileDetail exposing (Model, Msg(..), fetchProfile, fetchRepos, init, toNavKey, update, view)

import Browser.Navigation exposing (Key)
import Date
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as D
import Profile exposing (Profile, Repository, profileDecoder, repositoryDecoder)
import Routing exposing (Route(..))
import Time



-- MODEL


type Msg
    = Noop
    | SearchProfile String
    | GotProfile (Result Http.Error Profile)
    | GotRepositories (Result Http.Error (List Repository))


type ProfileState
    = Loading String
    | Fullfilled Profile


type alias Model =
    { navKey : Key
    , profile : ProfileState
    }



-- HTTP REQUEST


getUserEndpoint : String -> String
getUserEndpoint username =
    "https://api.github.com/users/" ++ username


fetchRepos : String -> Cmd Msg
fetchRepos username =
    Http.get
        { url = getUserEndpoint username ++ "/repos"
        , expect = Http.expectJson GotRepositories (D.list repositoryDecoder)
        }


fetchProfile : String -> Cmd Msg
fetchProfile query =
    Http.get
        { url = getUserEndpoint query
        , expect = Http.expectJson GotProfile profileDecoder
        }



-- INIT


init : String -> Key -> ( Model, Cmd Msg )
init username key =
    let
        model : Model
        model =
            { navKey = key
            , profile = Loading username
            }
    in
    ( model, fetchRepos username )



-- VIEW


renderItem : String -> String -> Html msg
renderItem content icon =
    if String.isEmpty content then
        text ""

    else
        p []
            [ i [ class icon, style "padding-right" "5px", style "color" "#fff" ] []
            , span [ style "color" "#c4c4c4" ] [ text content ]
            ]


renderMaybeItem : Maybe String -> String -> Html msg
renderMaybeItem maybeItem icon =
    case maybeItem of
        Just item ->
            renderItem item icon

        Nothing ->
            text ""


renderBadge : String -> Int -> Html msg
renderBadge title number =
    span [ class "badge text-bg-secondary", style "margin-right" "10px" ] [ text (title ++ String.fromInt number) ]


renderUserHeading : Profile -> Html msg
renderUserHeading profile =
    div [ class "p-3 mb-4 bg-dark rounded-3 text-white" ]
        [ div [ class "row" ]
            [ div [ class "col-sm-4" ]
                [ div []
                    [ img [ src profile.avatar_url, class "rounded-1 img-fluid" ] []
                    ]
                ]
            , div [ class "col-sm-8 d-flex flex-column justify-content-between" ]
                [ div []
                    [ h1 [ class "mb-4 display-6 mt-2" ] [ text (Maybe.withDefault profile.login profile.name) ]
                    , renderMaybeItem (Maybe.map (\loc -> "Location: " ++ loc) profile.location) "bi bi-geo-alt-fill"
                    , renderMaybeItem profile.blog "bi bi-globe"
                    , renderItem ("Followers: " ++ String.fromInt profile.followers) "bi bi-people-fill"
                    , renderItem ("Following: " ++ String.fromInt profile.following) "bi bi-person-heart"
                    , renderMaybeItem profile.bio "bi bi-bookmark-fill"
                    ]
                , div [ class "d-flex justify-content-between" ]
                    [ div [ class "d-flex align-items-end flex-wrap" ]
                        [ renderBadge "Public Repos: " profile.public_repos
                        , renderBadge "Public Gists: " profile.public_gists
                        ]
                    , a [ href profile.html_url, class "btn btn-outline-light" ] [ text "View on Github" ]
                    ]
                ]
            ]
        ]


dateFromString : String -> Result String Date.Date
dateFromString stringDate =
    stringDate
        |> String.split "T"
        |> List.head
        |> Maybe.withDefault "invalid-date"
        |> Date.fromIsoString


getDate : Result String String -> Html msg
getDate result =
    case result of
        Ok date ->
            small [ class "text-muted" ] [ span [] [ text "Created on: " ], b [] [ text date ] ]

        Err _ ->
            text ""


renderDate : String -> Html msg
renderDate stringDate =
    stringDate
        |> dateFromString
        |> Result.map (Date.format "MMMM y")
        |> getDate


getLanguageBadge : String -> Html msg
getLanguageBadge language =
    let
        classNames =
            case language of
                "Elm" ->
                    "badge text-bg-primary"

                "JavaScript" ->
                    "badge text-bg-warning"

                "TypeScript" ->
                    "badge text-bg-info"

                "Python" ->
                    "badge text-bg-success"

                "PHP" ->
                    "badge text-bg-danger"

                "Java" ->
                    "badge text-bg-danger"

                "C#" ->
                    "badge text-bg-light"

                _ ->
                    "badge text-bg-secondary"
    in
    div []
        [ span [ class classNames ]
            [ text language ]
        ]


toRepoCard : Repository -> Html msg
toRepoCard repo =
    div [ class "col-lg-6" ]
        [ div [ class "card mb-4" ]
            [ div [ class "card-body" ]
                [ div [ class "d-flex justify-content-between" ]
                    [ h5
                        [ class "card-title" ]
                        [ text repo.name ]
                    , repo.language
                        |> Maybe.map getLanguageBadge
                        |> Maybe.withDefault (text "")
                    ]
                , repo.description
                    |> Maybe.map
                        (\description ->
                            p [ class "card-text text-muted text-truncate" ]
                                [ text description ]
                        )
                    |> Maybe.withDefault (p [ style "min-height" "24px" ] [ text " " ])
                , renderDate repo.created_at
                ]
            ]
        ]


getDateWidthDefault : String -> Date.Date
getDateWidthDefault date =
    date
        |> dateFromString
        |> Result.withDefault
            (0
                |> Time.millisToPosix
                |> Date.fromPosix Time.utc
            )


compareDate : Repository -> Repository -> Order
compareDate a b =
    let
        dateA : Date.Date
        dateA =
            getDateWidthDefault a.created_at

        dateB : Date.Date
        dateB =
            getDateWidthDefault b.created_at
    in
    Date.compare dateB dateA


getRepos : Profile -> List (Html msg)
getRepos profile =
    case profile.repos of
        Just repos ->
            repos
                |> List.filter (\repo -> not repo.fork)
                |> List.sortWith compareDate
                |> List.map toRepoCard

        Nothing ->
            []


renderProfileRepos : Profile -> Html msg
renderProfileRepos profile =
    div [ class "pt-2" ]
        [ h4 [ class "text-muted mb-4" ] [ text "Repositories:" ]
        , div [ class "row" ] (getRepos profile)
        ]


renderBreadcrumbs : Profile -> Html msg
renderBreadcrumbs profile =
    nav [ attribute "aria-label" "breadcrumb" ]
        [ ol [ class "breadcrumb" ]
            [ li [ class "breadcrumb-item" ]
                [ text "Home"
                ]
            , li [ class "breadcrumb-item" ]
                [ text "Profile"
                ]
            , li [ attribute "aria-current" "page", class "breadcrumb-item active" ]
                [ text profile.login ]
            ]
        ]


view : ProfileState -> Html msg
view state =
    let
        content : List (Html msg)
        content =
            case state of
                Fullfilled profile ->
                    [ renderBreadcrumbs profile
                    , renderUserHeading profile
                    , renderProfileRepos profile
                    ]

                _ ->
                    [ div [] [ h4 [ class "text-muted" ] [ text "Search for a github profile" ] ]
                    ]
    in
    div [ class "pt-3" ] content



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRepositories result ->
            ( case result of
                Ok repos ->
                    { model
                        | profile =
                            case model.profile of
                                Fullfilled profile ->
                                    Fullfilled { profile | repos = Just repos }

                                Loading username ->
                                    Loading username
                    }

                Err _ ->
                    model
            , Cmd.none
            )

        GotProfile result ->
            case result of
                Ok data ->
                    ( model, fetchRepos data.login )

                Err _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- HELPERS


toNavKey : Model -> Key
toNavKey model =
    model.navKey
