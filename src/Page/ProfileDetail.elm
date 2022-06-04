module Page.ProfileDetail exposing (Model, Msg(..), fetchProfile, init, toNavKey, update, view)

import Api
import Browser.Navigation exposing (Key)
import Date
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as D
import Layout
import Profile as Profile exposing (Profile, Repository, profileDecoder, repositoryDecoder)
import Routing exposing (Route(..))
import Task exposing (Task)
import Time



-- MODEL


type Msg
    = Noop
    | SearchProfile String
    | GotProfile (Result Http.Error Profile)
    | GotLayoutMsg


type ProfileState
    = Loading String
    | Fullfilled Profile
    | Failed Http.Error


type alias Model =
    { navKey : Key
    , profile : ProfileState
    , layout : Layout.Model
    }



-- HTTP REQUEST


attachProfileRepos : Profile -> Task Http.Error Profile
attachProfileRepos profile =
    Api.fetch
        { endpoint = Api.Endpoint ("/users/" ++ profile.login ++ "/repos")
        , decoder = D.list repositoryDecoder
        , body = Nothing
        , headers = Nothing
        , method = Api.methods.get
        }
        |> Task.map (Profile.addRepos profile)


fetchProfile : String -> Task Http.Error Profile
fetchProfile username =
    Api.fetch
        { endpoint = Api.Endpoint ("/users/" ++ username)
        , decoder = profileDecoder
        , body = Nothing
        , headers = Nothing
        , method = Api.methods.get
        }
        |> Task.andThen attachProfileRepos



-- INIT


init : String -> Key -> Layout.Model -> ( Model, Cmd Msg )
init username key layout =
    let
        model : Model
        model =
            { navKey = key
            , profile = Loading username
            , layout = layout
            }
    in
    ( model, Task.attempt GotProfile (fetchProfile username) )



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
            small [] [ span [] [ text "Created on: " ], b [] [ text date ] ]

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
                        [ class "card-title text-secondary" ]
                        [ text repo.name ]
                    , repo.language
                        |> Maybe.map getLanguageBadge
                        |> Maybe.withDefault (text "")
                    ]
                , repo.description
                    |> Maybe.map
                        (\description ->
                            p [ class "card-text text-truncate" ]
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
        [ h4 [ class "mb-4" ] [ text "Repositories:" ]
        , div [ class "row" ] (getRepos profile)
        ]


renderBreadcrumbs : Profile -> Html msg
renderBreadcrumbs profile =
    nav [ attribute "aria-label" "breadcrumb" ]
        [ ol [ class "breadcrumb" ]
            [ li [ class "breadcrumb-item" ]
                [ a [ Routing.href Routing.Home ] [ text "Home" ]
                ]
            , li [ class "breadcrumb-item" ]
                [ text "Profile"
                ]
            , li [ attribute "aria-current" "page", class "breadcrumb-item active" ]
                [ text profile.login ]
            ]
        ]


renderPlaceholder : () -> Html msg
renderPlaceholder _ =
    div [ class "placeholder-glow" ]
        [ div [ class "row" ]
            [ p [] [ span [ class "placeholder col-4 rounded-1" ] [] ]
            ]
        , div [ class "p-3 mb-4 bg-dark rounded-3 text-white" ]
            [ div [ class "row" ]
                [ div [ class "col-sm-4" ]
                    [ div [ class "placeholder bg-light w-100 rounded-1", style "padding-bottom" "100%" ] []
                    ]
                , div [ class "col-sm-8 d-flex flex-column justify-content-between" ]
                    [ div []
                        [ h1 [ class "display-6  mb-4 mt-2" ] [ span [ class "placeholder col-8 rounded-1" ] [] ]
                        , p [] [ span [ class "placeholder col-6 rounded-1" ] [] ]
                        , p [] [ span [ class "placeholder col-4 rounded-1" ] [] ]
                        , p [] [ span [ class "placeholder col-5 rounded-1" ] [] ]
                        ]
                    , div [ class "d-flex justify-content-between flex-wrap w-100" ]
                        [ div [ class "d-block placeholder rounded-1 bg-light", style "width" "30%" ] []
                        , div [ class "d-block placeholder rounded-1 bg-light", style "width" "20%" ] []
                        ]
                    ]
                ]
            ]
        , div
            [ class "row" ]
            [ div [ class "col-6 pt-4" ]
                [ h3 [] [ span [ class "placeholder col-8 rounded-1" ] [] ]
                , p [] [ span [ class "placeholder col-12 rounded-1" ] [] ]
                , p [] [ span [ class "placeholder col-12 rounded-1" ] [] ]
                ]
            , div [ class "col-6 pt-4" ]
                [ h3 [] [ span [ class "placeholder col-8 rounded-1" ] [] ]
                , p [] [ span [ class "placeholder col-12 rounded-1" ] [] ]
                , p [] [ span [ class "placeholder col-12 rounded-1" ] [] ]
                ]
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

                Loading _ ->
                    [ renderPlaceholder () ]

                _ ->
                    [ nav [ attribute "aria-label" "breadcrumb" ]
                        [ ol [ class "breadcrumb" ]
                            [ li [ class "breadcrumb-item" ]
                                [ text "Home"
                                ]
                            , li [ class "breadcrumb-item" ]
                                [ text "Profile"
                                ]
                            , li [ attribute "aria-current" "page", class "breadcrumb-item active" ]
                                [ text "Unknown" ]
                            ]
                        ]
                    , div [ class "alert alert-danger", attribute "role" "alert" ]
                        [ div [ class "d-flex align-items-center" ]
                            [ i [ class "fs-4 d-inline-block pe-2 bi bi-exclamation-circle-fill" ] []
                            , text "Github user not found."
                            ]
                        ]
                    , div [ class "w-100 d-flex justify-content-center p-4" ]
                        [ i [ class "text-danger bi bi-github", style "font-size" "12rem" ] [] ]
                    ]
    in
    div [ class "pt-3" ] content



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotProfile result ->
            case result of
                Ok data ->
                    ( { model | profile = Fullfilled data }, Cmd.none )

                Err error ->
                    ( { model | profile = Failed error }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- HELPERS


toNavKey : Model -> Key
toNavKey model =
    model.navKey
