module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Navigation exposing (Key)
import Html exposing (..)
import Html.Attributes exposing (..)
import Layout
import Page.Favorites as FavoritesPage
import Page.Home as HomePage
import Page.ProfileDetail as ProfileDetailPage
import Page.Search as SearchPage
import Routing exposing (Route(..), parseUrlToRoute)
import Url exposing (Url)



-- MODEL


type Msg
    = SendUserToExternalUrl String
    | UrlChanged Url
    | UrlChangeRequested Url
    | GotHomePageMsg
    | GotSearchPageMsg SearchPage.Msg
    | GotFavoritesPageMsg FavoritesPage.Msg
    | GotProfileDetailPageMsg ProfileDetailPage.Msg
    | GotLayoutMsg Layout.Msg


type Model
    = Home HomePage.Model
    | Search SearchPage.Model
    | Favorites FavoritesPage.Model
    | ProfileDetail ProfileDetailPage.Model


type alias Flags =
    { storedProfile : Maybe String
    }


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


getKeyFromModel : Model -> Key
getKeyFromModel model =
    case model of
        Home subModel ->
            subModel.layout.navKey

        ProfileDetail subModel ->
            subModel.layout.navKey

        Search subModel ->
            subModel.layout.navKey

        Favorites subModel ->
            subModel.layout.navKey


mapToPage : Route -> Layout.Model -> ( Model, Cmd Msg )
mapToPage route layout =
    let
        ( model, cmd ) =
            case route of
                Routing.ProfileDetail username ->
                    ProfileDetailPage.init username { layout | activeRoute = Routing.ProfileDetail username }
                        |> updateWith ProfileDetail GotProfileDetailPageMsg

                Routing.Search query entity ->
                    SearchPage.init
                        { layout
                            | activeRoute = Routing.Search query entity
                            , searchEntity = Layout.getSearchEntityFromQuery entity
                        }
                        |> updateWith Search GotSearchPageMsg

                Routing.Favorites ->
                    FavoritesPage.init { layout | activeRoute = Routing.Favorites }
                        |> updateWith Favorites GotFavoritesPageMsg

                Routing.Home ->
                    HomePage.init { layout | activeRoute = Routing.Home }
                        |> updateWith Home (\_ -> GotHomePageMsg)
    in
    ( model, cmd )



-- INIT


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    mapToPage (parseUrlToRoute url) (Layout.init key)



-- UPDATE


getLayoutModel : Model -> Layout.Model
getLayoutModel model =
    case model of
        Home subModel ->
            subModel.layout

        Search subModel ->
            subModel.layout

        Favorites subModel ->
            subModel.layout

        ProfileDetail subModel ->
            subModel.layout


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        key =
            getKeyFromModel model
    in
    case ( msg, model ) of
        ( SendUserToExternalUrl url, _ ) ->
            ( model, Navigation.load url )

        ( UrlChangeRequested url, _ ) ->
            ( model, Navigation.pushUrl key (Url.toString url) )

        ( UrlChanged url, currentPage ) ->
            currentPage
                |> getLayoutModel
                |> (\layout -> { layout | navKey = key })
                |> mapToPage (parseUrlToRoute url)

        ( GotProfileDetailPageMsg subMsg, ProfileDetail subModel ) ->
            ProfileDetailPage.update subMsg subModel
                |> updateWith ProfileDetail GotProfileDetailPageMsg

        ( GotSearchPageMsg subMsg, Search subModel ) ->
            SearchPage.update subMsg subModel
                |> updateWith Search GotSearchPageMsg

        ( GotLayoutMsg subMsg, currentPage ) ->
            case currentPage of
                ProfileDetail p ->
                    Layout.update subMsg (getLayoutModel currentPage)
                        |> updateWith (\m -> ProfileDetail { p | layout = m }) GotLayoutMsg

                Home p ->
                    Layout.update subMsg (getLayoutModel currentPage)
                        |> updateWith (\m -> Home { p | layout = m }) GotLayoutMsg

                Favorites p ->
                    Layout.update subMsg (getLayoutModel currentPage)
                        |> updateWith (\m -> Favorites { p | layout = m }) GotLayoutMsg

                Search p ->
                    Layout.update subMsg (getLayoutModel currentPage)
                        |> updateWith (\m -> Search { p | layout = m }) GotLayoutMsg

        _ ->
            ( model, Cmd.none )



-- VIEW


renderLayout : Model -> (Layout.Model -> Html Layout.Msg) -> Html Msg
renderLayout model render =
    model
        |> getLayoutModel
        |> render
        |> Html.map GotLayoutMsg


view : Model -> Document Msg
view model =
    { title = "Github Finder"
    , body =
        [ renderLayout model Layout.header
        , div [ style "background-color" "#f2f2f2", style "position" "relative", style "padding-top" "60px" ]
            [ case model of
                Home _ ->
                    main_ [ class "container-fluid p-0" ]
                        [ HomePage.view () ]

                ProfileDetail subModel ->
                    main_ [ class "container pb-4" ]
                        [ ProfileDetailPage.view subModel.profile ]

                Search subModel ->
                    main_ [ class "container pb-4" ]
                        [ Html.map GotSearchPageMsg (SearchPage.view subModel) ]

                Favorites _ ->
                    main_ [ class "container pb-4" ]
                        [ FavoritesPage.view () ]
            ]
        , renderLayout model Layout.footer
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map GotLayoutMsg (Layout.subscriptions (getLayoutModel model))
        ]


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = UrlChanged
        }
