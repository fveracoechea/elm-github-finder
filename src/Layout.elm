module Layout exposing (headerView)

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
