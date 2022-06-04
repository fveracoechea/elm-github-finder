module Page.Search exposing (Model, Msg(..), init, toNavKey, view)

import Browser exposing (UrlRequest(..))
import Browser.Navigation exposing (Key)
import Html exposing (..)
import Html.Attributes exposing (..)
import Layout
import Profile exposing (Profile)



-- MODEL


type alias Model =
    { navKey : Browser.Navigation.Key
    , query : String
    , profiles : Maybe (List Profile)
    , layout : Layout.Model
    }


type Msg
    = Noop



-- INIT


init : Key -> Layout.Model -> ( Model, Cmd msg )
init key layout =
    ( { navKey = key, query = "", profiles = Nothing, layout = layout }, Cmd.none )



--VIEW


view : () -> Html msg
view _ =
    div [ class "row" ]
        [ h2 [] [ text "Search page" ]
        ]



-- HELPERS


toNavKey : Model -> Key
toNavKey model =
    model.navKey
