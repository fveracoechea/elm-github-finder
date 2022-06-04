module Page.Home exposing (Model, init, toNavKey, view)

import Browser exposing (UrlRequest(..))
import Browser.Navigation exposing (Key)
import Html exposing (..)
import Html.Attributes exposing (..)
import Layout


type alias Model =
    { navKey : Browser.Navigation.Key
    , layout : Layout.Model
    }


init : Key -> Layout.Model -> ( Model, Cmd msg )
init key layout =
    ( { navKey = key, layout = layout }, Cmd.none )


view : () -> Html msg
view _ =
    div [ class "homepage-wrapper" ]
        [ div [ class "banner" ]
            [ img [ src "/homepage-image.jpg", class "img-fluid w-100 banner-image" ] []
            , div [ class "banner-overlay" ]
                [ div [ class "banner-content p-4 text-light text-center" ]
                    [ h1 [ class "display-2" ] [ text "Where the world searches software" ]
                    , p [ class "lead fs-4" ]
                        [ text """
                            Millions of developers and companies build, ship,
                            and maintain their software on GitHub—the largest and most
                            advanced development platform in the world.
                            """
                        ]
                    , Html.form [ class "d-flex justify-content-center", attribute "role" "search" ]
                        [ input
                            [ class "form-control me-2 text-light banner-input border-primary "
                            , placeholder "Profiles, Repositories and more..."
                            , attribute "aria-label" "Search"
                            , style "max-width" "265px"
                            ]
                            []
                        , button
                            [ class "btn btn-primary"
                            , type_ "submit"
                            , attribute "aria-label" "submit search"
                            , style "min-width" "8rem"
                            ]
                            [ text "Search now!" ]
                        ]
                    ]
                ]
            ]
        ]



-- HELPERS


toNavKey : Model -> Key
toNavKey model =
    model.navKey
