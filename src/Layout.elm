module Layout exposing (Model, Msg(..), footer, header, init, subscriptions, update)

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


type alias NavigationHeight =
    { initial : Int
    , current : Int
    }


type alias Model =
    { isMobile : Bool
    , isNavOpen : Bool
    , query : String
    , navKey : Navigation.Key
    , activeRoute : Routing.Route
    , navigation : NavigationHeight
    }



-- INIT


init : Navigation.Key -> Model
init key =
    { isMobile = True
    , isNavOpen = True
    , query = ""
    , navKey = key
    , activeRoute = Routing.Home
    , navigation =
        { initial = 140
        , current = 0
        }
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
            let
                current =
                    model.navigation.current

                initial =
                    model.navigation.initial

                isNavOpen =
                    not model.isNavOpen
            in
            if not model.isNavOpen && current > 0 then
                let
                    nav =
                        { initial = initial
                        , current = 0
                        }
                in
                ( { model | navigation = nav, isNavOpen = isNavOpen }, Cmd.none )

            else if model.isNavOpen && current < initial then
                let
                    nav =
                        { initial = initial
                        , current = initial
                        }
                in
                ( { model | navigation = nav, isNavOpen = isNavOpen }, Cmd.none )

            else
                ( { model | isNavOpen = isNavOpen }, Cmd.none )

        GotSearchRequest ->
            ( model
            , Routing.Search
                |> Routing.routeToString
                |> Navigation.pushUrl model.navKey
            )



-- VIEW


getNavHeight : Model -> Attribute msg
getNavHeight model =
    if model.isMobile then
        style "height" (String.fromInt model.navigation.current ++ "px")

    else
        style "height" "auto"


header : Model -> Html Msg
header model =
    Html.header []
        [ nav [ class "navbar navbar-expand-lg navbar-dark bg-dark" ]
            [ div [ class "container" ]
                [ a [ class "navbar-brand", Routing.href Routing.Home ]
                    [ i [ class "bi bi-github", style "margin-right" "10px", style "font-size" "1.43rem" ]
                        []
                    , text "Github Finder"
                    ]
                , button [ onClick MobileNavToggled, class "navbar-toggler", type_ "button" ] [ span [ class "navbar-toggler-icon" ] [] ]
                , div [ class "collapse navbar-collapse justify-content-between", id navID, getNavHeight model ]
                    [ ul [ class "navbar-nav" ]
                        [ li [ class "nav-item" ]
                            [ a
                                [ class "nav-link"
                                , Routing.href Routing.Search
                                , Routing.isActiveLink Routing.Search model.activeRoute
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
                        [ a [ Routing.href Routing.Search ]
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
            , p [] [ text "?? 2022 Copyright - Francisco Veracoechea" ]
            ]
        ]



-- SUB


subscriptions : Model -> Sub Msg
subscriptions _ =
    onResize (\w _ -> WindowResized w)
