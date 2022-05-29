module Layout exposing (headerView)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types exposing (..)


headerView : Model -> Html Msg
headerView _ =
    header []
        [ nav [ class "navbar navbar-expand-lg navbar-dark bg-dark" ]
            [ div [ class "container" ]
                [ a [ class "navbar-brand", href "#" ]
                    [ text "Github Finder" ]
                , div [ id "navbarSupportedContent" ]
                    [ div [ class "navbar-nav me-auto my-2 my-lg-0 navbar-nav-scroll" ] []
                    , Html.form [ class "d-flex", attribute "role" "search", onSubmit OnSearch ]
                        [ input
                            [ attribute "aria-label" "Search"
                            , class "form-control me-2"
                            , placeholder "Search for username"
                            , type_ "search"
                            , onInput SetQuery
                            ]
                            []
                        , button [ class "btn btn-primary", type_ "submit" ]
                            [ text "Search" ]
                        ]
                    ]
                ]
            ]
        ]
