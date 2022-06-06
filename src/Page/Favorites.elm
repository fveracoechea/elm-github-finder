module Page.Favorites exposing (Model, Msg(..), init, view)

import Browser exposing (UrlRequest(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Layout



-- MODEL


type alias Model =
    { layout : Layout.Model
    , query : String
    }


type Msg
    = Noop



-- INIT


init : Layout.Model -> ( Model, Cmd msg )
init layout =
    ( { query = "", layout = layout }, Cmd.none )



--VIEW


view : () -> Html msg
view _ =
    div [ class "row" ]
        [ h2 [] [ text "Favorites page" ]
        ]
