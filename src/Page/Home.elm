module Page.Home exposing (Model, init, toNavKey, view)

import Browser exposing (UrlRequest(..))
import Browser.Navigation exposing (Key)
import Html exposing (..)


type alias Model =
    { navKey : Browser.Navigation.Key
    }


init : Key -> ( Model, Cmd msg )
init key =
    ( { navKey = key }, Cmd.none )


view : () -> Html msg
view _ =
    div [] [ text "Homepage" ]



-- HELPERS


toNavKey : Model -> Key
toNavKey model =
    model.navKey
