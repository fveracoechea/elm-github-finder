module Layout exposing (Model, Msg(..), footer, header, init, initialModel, subscriptions, update)

import Browser.Events exposing (onResize)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Routing



-- Msg


type Msg
    = MobileNavToggled
    | WindowResized Int



-- MODEL


navID : String
navID =
    "header-navigation"


type alias NavigationHeight =
    { initial : Int
    , current : Int
    }


initialModel : Model
initialModel =
    { isMobile = True
    , isNavOpen = True
    , navigation =
        { initial = 140
        , current = 0
        }
    }


type alias Model =
    { isMobile : Bool
    , isNavOpen : Bool
    , navigation : NavigationHeight
    }



-- INIT


init : () -> Cmd Msg
init _ =
    Cmd.none



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "model" model
    in
    case msg of
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



-- VIEW


getNavHeight : Model -> Attribute msg
getNavHeight model =
    if model.isMobile then
        style "height" (String.fromInt model.navigation.current ++ "px")

    else
        style "height" "auto"


header : toggleMsg -> Model -> Html toggleMsg
header toggleMsg model =
    Html.header []
        [ nav [ class "navbar navbar-expand-lg navbar-dark bg-dark" ]
            [ div [ class "container" ]
                [ a [ class "navbar-brand", Routing.href Routing.Home ]
                    [ i [ class "bi bi-github", style "margin-right" "10px", style "font-size" "1.43rem" ]
                        []
                    , text "Github Finder"
                    ]
                , button [ onClick toggleMsg, class "navbar-toggler", type_ "button" ] [ span [ class "navbar-toggler-icon" ] [] ]
                , div [ class "collapse navbar-collapse justify-content-between", id navID, getNavHeight model ]
                    [ ul [ class "navbar-nav" ]
                        [ li [ class "nav-item" ]
                            [ a [ class "nav-link", Routing.href Routing.Search ]
                                [ i [ class "bi bi-search me-2" ] []
                                , text "Search"
                                ]
                            ]
                        , li [ class "nav-item" ]
                            [ a [ class "nav-link", Routing.href Routing.Favorites ]
                                [ i [ class "bi bi-heart-fill me-2" ] [], text "Favorites" ]
                            ]
                        ]
                    , Html.form [ class "d-flex", attribute "role" "search" ]
                        [ input
                            [ class "form-control me-2"
                            , placeholder "What are you looking for?"
                            , attribute "aria-label" "Search"
                            , style "min-width" "221px"
                            ]
                            []
                        , button
                            [ class "btn btn-outline-primary"
                            , type_ "submit"
                            , attribute "aria-label" "submit search"
                            ]
                            [ text "Search" ]
                        ]
                    ]
                ]
            ]
        ]


footer : () -> Html msg
footer _ =
    Html.footer [ class "page-footer font-small indigo" ]
        [ div [ class "container" ]
            [ div [ class "row d-flex text-center justify-content-center mb-md-0 mb-4" ]
                [ div [ class "col-md-8 col-12 mt-5" ]
                    [ h3 [ class "display-6" ] [ text "Make your contribution" ]
                    , p [ attribute "style" "line-height: 1.7rem" ]
                        [ text
                            """
                            Small experiments, inspired inventions,
                            and the software everyone depends on—the code you write on
                            GitHub can reach one codebase or millions.
                            """
                        ]
                    ]
                ]
            , hr [ class "rgba-white-light" ]
                []
            , div [ class "row text-center d-flex justify-content-center pt-5 mb-3" ]
                [ div [ class "col-md-2 mb-3" ]
                    [ h6 [ class "text-uppercase" ]
                        [ a [ href "#!" ]
                            [ text "About us" ]
                        ]
                    ]
                , div [ class "col-md-2 mb-3" ]
                    [ h6 [ class "text-uppercase" ]
                        [ a [ href "#!" ]
                            [ text "Products" ]
                        ]
                    ]
                , div [ class "col-md-2 mb-3" ]
                    [ h6 [ class "text-uppercase" ]
                        [ a [ href "#!" ]
                            [ text "Awards" ]
                        ]
                    ]
                , div [ class "col-md-2 mb-3" ]
                    [ h6 [ class "text-uppercase" ]
                        [ a [ href "#!" ]
                            [ text "Help" ]
                        ]
                    ]
                , div [ class "col-md-2 mb-3" ]
                    [ h6 [ class "text-uppercase" ]
                        [ a [ href "#!" ]
                            [ text "Contact" ]
                        ]
                    ]
                ]
            ]
        , div [ class "footer-copyright text-center py-3" ]
            [ text "© 2020 Copyright - Francisco Veracoechea"
            ]
        ]



-- SUB


subscriptions : Model -> Sub Msg
subscriptions _ =
    onResize (\w _ -> WindowResized w)
