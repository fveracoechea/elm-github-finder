module Page.Home exposing (Model, init, view)

import Browser exposing (UrlRequest(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Layout
import Routing


type alias Model =
    { layout : Layout.Model
    }


init : Layout.Model -> ( Model, Cmd msg )
init layout =
    ( { layout = layout }, Cmd.none )


view : () -> Html msg
view _ =
    div [ class "homepage-wrapper" ]
        [ div [ class "banner" ]
            [ div [ class "banner-image-wrapper" ]
                [ img [ src "/homepage-image.jpg", class "img-fluid w-100 banner-image" ] []
                ]
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
                    , a
                        [ class "btn btn-outline-primary"
                        , type_ "submit"
                        , attribute "aria-label" "submit search"
                        , style "min-width" "8rem"
                        , Routing.href Routing.Search
                        ]
                        [ text "SEARCH NOW!" ]
                    ]
                ]
            ]
        ]
