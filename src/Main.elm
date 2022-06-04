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
            HomePage.toNavKey subModel

        ProfileDetail subModel ->
            ProfileDetailPage.toNavKey subModel

        Search subModel ->
            SearchPage.toNavKey subModel

        Favorites subModel ->
            FavoritesPage.toNavKey subModel


mapToPage : Route -> Key -> Layout.Model -> ( Model, Cmd Msg )
mapToPage route key layout =
    let
        ( model, cmd ) =
            case route of
                Routing.ProfileDetail username ->
                    ProfileDetailPage.init username key { layout | activeRoute = Routing.ProfileDetail username }
                        |> updateWith ProfileDetail GotProfileDetailPageMsg

                Routing.Search ->
                    SearchPage.init key { layout | activeRoute = Routing.Search }
                        |> updateWith Search GotSearchPageMsg

                Routing.Favorites ->
                    FavoritesPage.init key { layout | activeRoute = Routing.Favorites }
                        |> updateWith Favorites GotFavoritesPageMsg

                Routing.Home ->
                    HomePage.init key { layout | activeRoute = Routing.Home }
                        |> updateWith Home (\_ -> GotHomePageMsg)
    in
    ( model, Cmd.batch [ cmd, Cmd.map GotLayoutMsg (Layout.init ()) ] )



-- INIT


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    mapToPage (parseUrlToRoute url) key Layout.initialModel



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
    case ( msg, model ) of
        ( SendUserToExternalUrl url, _ ) ->
            ( model, Navigation.load url )

        ( UrlChangeRequested url, _ ) ->
            ( model, Navigation.pushUrl (getKeyFromModel model) (Url.toString url) )

        ( UrlChanged url, currentPage ) ->
            mapToPage (parseUrlToRoute url) (getKeyFromModel model) (getLayoutModel currentPage)

        ( GotProfileDetailPageMsg subMsg, ProfileDetail subModel ) ->
            ProfileDetailPage.update subMsg subModel
                |> updateWith ProfileDetail GotProfileDetailPageMsg

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


view : Model -> Document Msg
view model =
    { title = "Github Finder"
    , body =
        [ Layout.header (GotLayoutMsg Layout.MobileNavToggled) (getLayoutModel model)
        , div [ style "min-height" "20vh", style "background-color" "#f2f2f2", style "position" "relative" ]
            [ case model of
                Home _ ->
                    main_ [ class "container-fluid p-0" ]
                        [ HomePage.view () ]

                ProfileDetail subModel ->
                    main_ [ class "container" ]
                        [ ProfileDetailPage.view subModel.profile ]

                Search _ ->
                    main_ [ class "container" ]
                        [ SearchPage.view () ]

                Favorites _ ->
                    main_ [ class "container" ]
                        [ FavoritesPage.view () ]
            ]
        , Layout.footer ()
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
