module Page.Favorites exposing (Model, Msg(..), init, toNavKey, view)

import Browser exposing (UrlRequest(..))
import Browser.Navigation exposing (Key)
import Html exposing (..)
import Html.Attributes exposing (..)
import Layout



-- MODEL


type alias Model =
    { navKey : Browser.Navigation.Key
    , layout : Layout.Model
    , query : String
    }


type Msg
    = Noop



-- INIT


init : Key -> Layout.Model -> ( Model, Cmd msg )
init key layout =
    ( { navKey = key, query = "", layout = layout }, Cmd.none )



--VIEW


view : () -> Html msg
view _ =
    div [ class "row" ]
        [ h2 [] [ text "Favorites page" ]
        ]



-- HELPERS


toNavKey : Model -> Key
toNavKey model =
    model.navKey
