module Page.Home exposing (Model, init, toNavKey, view)

import Browser exposing (UrlRequest(..))
import Browser.Navigation exposing (Key)
import Html exposing (..)
import Html.Attributes exposing (..)


type alias Model =
    { navKey : Browser.Navigation.Key
    }


init : Key -> ( Model, Cmd msg )
init key =
    ( { navKey = key }, Cmd.none )


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
                    , button [ class "btn-lg btn btn-outline-primary me-4" ] [ text "Profiles" ]
                    , button [ class "btn-lg btn btn-outline-light" ] [ text "Repositories" ]
                    ]
                ]
            ]
        ]



-- HELPERS


toNavKey : Model -> Key
toNavKey model =
    model.navKey
