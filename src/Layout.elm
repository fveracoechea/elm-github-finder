module Layout exposing (Model, Msg(..), SearchEntity(..), footer, header, init, subscriptions, update)

import Browser.Events exposing (onResize)
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Routing



-- Msg


type Msg
    = MobileNavToggled
    | WindowResized Int
    | GotQuery String
    | GotSearchRequest



-- MODEL


navID : String
navID =
    "header-navigation"


type SearchEntity
    = Repository
    | Profile
    | Topic


type alias Model =
    { isMobile : Bool
    , isNavOpen : Bool
    , query : String
    , searchEntity : Maybe SearchEntity
    , navKey : Navigation.Key
    , activeRoute : Routing.Route
    }



-- INIT


init : Navigation.Key -> Model
init key =
    { isMobile = True
    , isNavOpen = False
    , query = ""
    , searchEntity = Nothing
    , navKey = key
    , activeRoute = Routing.Home
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotQuery query ->
            ( { model | query = query }, Cmd.none )

        WindowResized width ->
            ( { model | isMobile = width <= 991 }
            , Cmd.none
            )

        MobileNavToggled ->
            ( { model | isNavOpen = not model.isNavOpen }, Cmd.none )

        GotSearchRequest ->
            let
                entity =
                    case model.searchEntity of
                        Just value ->
                            case value of
                                Profile ->
                                    Just "profile"

                                Repository ->
                                    Just "repository"

                                Topic ->
                                    Just "topic"

                        Nothing ->
                            Just "profile"

                query =
                    if String.isEmpty model.query then
                        Nothing

                    else
                        Just model.query
            in
            ( model
            , Routing.Search query entity
                |> Routing.routeToString
                |> Navigation.pushUrl model.navKey
            )



-- VIEW


setActiveNavbar : String -> Model -> Attribute msg
setActiveNavbar baseClasses model =
    if model.isNavOpen then
        class <| baseClasses ++ " active"

    else
        class baseClasses


header : Model -> Html Msg
header model =
    Html.header []
        [ div [ setActiveNavbar "backdrop" model, onClick MobileNavToggled ] []
        , nav [ class "navbar navbar-expand-lg navbar-dark bg-dark fixed-top" ]
            [ div [ class "container" ]
                [ a [ class "navbar-brand", Routing.href Routing.Home ]
                    [ i [ class "bi bi-github", style "margin-right" "10px", style "font-size" "1.43rem" ]
                        []
                    , text "Github Finder"
                    ]
                , button
                    [ onClick MobileNavToggled, class "navbar-toggler", type_ "button" ]
                    [ span [ class "navbar-toggler-icon" ] [] ]
                , div [ id navID, setActiveNavbar "collapse navbar-collapse justify-content-between" model ]
                    [ ul [ class "navbar-nav" ]
                        [ li [ class "nav-item" ]
                            [ a
                                [ class "nav-link"
                                , Routing.href (Routing.Search Nothing Nothing)
                                , Routing.isActiveLink (Routing.Search Nothing Nothing) model.activeRoute
                                ]
                                [ i [ class "bi bi-search me-2" ] []
                                , text "Search"
                                ]
                            ]
                        , li [ class "nav-item" ]
                            [ a
                                [ class "nav-link"
                                , Routing.href Routing.Favorites
                                , Routing.isActiveLink Routing.Favorites model.activeRoute
                                ]
                                [ i [ class "bi bi-heart-fill me-2" ] [], text "Favorites" ]
                            ]
                        ]
                    , Html.form [ class "d-flex", attribute "role" "search", onSubmit GotSearchRequest ]
                        [ input
                            [ class "form-control me-2"
                            , placeholder "What are you looking for?"
                            , attribute "aria-label" "Search"
                            , style "min-width" "221px"
                            , onInput GotQuery
                            ]
                            []
                        , button
                            [ class "btn btn-primary text-light"
                            , type_ "submit"
                            , attribute "aria-label" "submit search"
                            ]
                            [ text "Search" ]
                        ]
                    ]
                ]
            ]
        ]


footer : Model -> Html Msg
footer _ =
    Html.footer [ class "footer page-footer font-small bg-secondary" ]
        [ div [ class "container" ]
            [ div [ class "row d-flex text-center justify-content-center mb-md-0 mb-4" ]
                [ div [ class "col-md-8 col-12 mt-5" ]
                    [ h3 [ class "display-6" ] [ text "Make your contribution" ]
                    , p [ attribute "style" "line-height: 1.7rem" ]
                        [ text
                            """
                            Small experiments, inspired inventions,
                            and the software everyone depends on. The code you write on
                            GitHub can reach one codebase or millions.
                            """
                        ]
                    ]
                ]
            , hr [ class "rgba-white-light" ]
                []
            , div [ class "row text-center d-flex justify-content-center pt-5 mb-3 footer-links" ]
                [ div [ class "col-md-2 mb-3" ]
                    [ h6 [ class "text-uppercase" ]
                        [ a [ Routing.href Routing.Home ]
                            [ text "Home" ]
                        ]
                    ]
                , div [ class "col-md-2 mb-3" ]
                    [ h6 [ class "text-uppercase" ]
                        [ a [ Routing.href (Routing.Search Nothing Nothing) ]
                            [ text "Search" ]
                        ]
                    ]
                , div [ class "col-md-2 mb-3" ]
                    [ h6 [ class "text-uppercase" ]
                        [ a [ Routing.href Routing.Favorites ]
                            [ text "Favorites" ]
                        ]
                    ]
                ]
            ]
        , div [ class "footer-copyright text-center p-3" ]
            [ p []
                [ text "This project is bootstrapped with "
                , a [ href "https://github.com/halfzebra/create-elm-app" ]
                    [ text "Create Elm App" ]
                , text " and uses "
                , a [ href "https://getbootstrap.com/" ]
                    [ text "Bootstrap" ]
                , text " as styling framework."
                ]
            , p [] [ text "© 2022 Copyright - Francisco Veracoechea" ]
            ]
        ]



-- SUB


subscriptions : Model -> Sub Msg
subscriptions _ =
    onResize (\w _ -> WindowResized w)
