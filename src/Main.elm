module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Navigation exposing (Key)
import Html exposing (..)
import Html.Attributes exposing (..)
import Layout
import Page.Home as HomePage
import Page.ProfileDetail as ProfileDetailPage
import Routing exposing (Route(..), parseUrlToRoute)
import Url exposing (Url)



-- MODEL


type Msg
    = Noop
    | OnSearch
    | SetQuery String
    | SendUserToExternalUrl String
    | UrlChanged Url
    | UrlChangeRequested Url
    | GotProfileDetailPageMsg ProfileDetailPage.Msg
    | GotHomePageMsg


type Model
    = Home HomePage.Model
    | ProfileDetail ProfileDetailPage.Model


type alias Flags =
    { storedProfile : Maybe String
    }


initWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
initWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


getKeyFromModel : Model -> Key
getKeyFromModel model =
    case model of
        Home subModel ->
            HomePage.toNavKey subModel

        ProfileDetail subModel ->
            ProfileDetailPage.toNavKey subModel


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    case parseUrlToRoute url of
        Routing.ProfileDetail username ->
            ProfileDetailPage.init username key
                |> initWith ProfileDetail GotProfileDetailPageMsg

        -- ProfileDetailPage.init username url key
        Routing.Home ->
            HomePage.init key
                |> initWith Home (\_ -> GotHomePageMsg)



-- UPDATE


changeRoute : Route -> Model -> ( Model, Cmd Msg )
changeRoute route model =
    let
        key =
            getKeyFromModel model
    in
    case route of
        Routing.ProfileDetail username ->
            ProfileDetailPage.init username key
                |> initWith ProfileDetail GotProfileDetailPageMsg

        Routing.Home ->
            HomePage.init key
                |> initWith Home (\_ -> GotHomePageMsg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- OnSearch ->
        --     let
        --         command =
        --             if String.isEmpty model.query || String.length model.query < 3 then
        --                 Cmd.none
        --             else
        --                 Api.fetchProfile model.query
        --     in
        --     ( model, command )
        -- SetQuery newQuery ->
        --     ( { model | query = newQuery }, Cmd.none )
        SendUserToExternalUrl url ->
            ( model, Navigation.load url )

        UrlChangeRequested url ->
            ( model, Navigation.pushUrl (getKeyFromModel model) (Url.toString url) )

        UrlChanged url ->
            changeRoute (parseUrlToRoute url) model

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Github Finder"
    , body =
        [ Layout.headerView ()
        , div [ style "min-height" "60vh", style "background-color" "#f2f2f2" ]
            [ main_ [ class "container" ]
                [ case model of
                    ProfileDetail subModel ->
                        ProfileDetailPage.view subModel.profile

                    Home _ ->
                        HomePage.view ()
                ]
            ]
        , Layout.footerView ()
        ]
    }



-- onUrlRequest


onUrlRequest : UrlRequest -> Msg
onUrlRequest urlRequest =
    case urlRequest of
        External externalUrl ->
            SendUserToExternalUrl externalUrl

        Internal url ->
            UrlChangeRequested url



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = onUrlRequest
        , onUrlChange = UrlChanged
        }
