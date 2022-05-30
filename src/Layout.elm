module Layout exposing (contentView, headerView)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Page.Profile as ProfilePage
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
                            , placeholder "Type a username..."
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


contentView : Model -> Html Msg
contentView model =
    div [ style "min-height" "100vh", style "background-color" "#f2f2f2" ]
        [ main_ [ class "container" ]
            [ ProfilePage.view model.profile
            ]
        ]
