module Layout exposing (footerView, headerView)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Routing


headerView : () -> Html msg
headerView _ =
    header []
        [ nav [ class "navbar navbar-expand-lg navbar-dark bg-dark" ]
            [ div [ class "container" ]
                [ a [ class "navbar-brand", href "/" ]
                    [ i [ class "bi bi-github", style "margin-right" "10px", style "font-size" "1.43rem" ]
                        []
                    , text "Github Finder"
                    ]
                , button [ class "navbar-toggler", type_ "button" ] [ span [ class "navbar-toggler-icon" ] [] ]
                , ul [ class "collapse navbar-collapse navbar-nav justify-content-end" ]
                    [ li [ class "nav-item" ]
                        [ a [ attribute "aria-current" "page", class "nav-link active", Routing.href Routing.Home ]
                            [ text "Home" ]
                        ]
                    , li [ class "nav-item" ]
                        [ a [ class "nav-link", href "#" ]
                            [ text "Favorites" ]
                        ]
                    , li [ class "nav-item" ]
                        [ a [ class "nav-link", Routing.href (Routing.ProfileDetail "test") ]
                            [ text "Profiles" ]
                        ]
                    , li [ class "nav-item" ]
                        [ a [ class "nav-link", href "#" ]
                            [ text "Repositories" ]
                        ]
                    ]
                ]
            ]
        ]


footerView : () -> Html msg
footerView _ =
    footer [ class "page-footer font-small indigo" ]
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
