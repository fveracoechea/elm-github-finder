module Routing exposing (Route(..), href, parseUrlToRoute, routeToPieces, routeToString)

import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>))


type Route
    = Home
    | Search
    | Favorites
    | ProfileDetail String


routeParser : Parser.Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Search (Parser.s "search")
        , Parser.map Favorites (Parser.s "favorites")
        , Parser.map ProfileDetail (Parser.s "profiles" </> Parser.string)
        ]


parseUrlToRoute : Url -> Route
parseUrlToRoute url =
    Maybe.withDefault Home (Parser.parse routeParser url)


routeToString : Route -> String
routeToString page =
    "/" ++ String.join "/" (routeToPieces page)


routeToPieces : Route -> List String
routeToPieces page =
    case page of
        Home ->
            []

        Search ->
            [ "search" ]

        Favorites ->
            [ "favorites" ]

        ProfileDetail username ->
            [ "profiles", username ]


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)
