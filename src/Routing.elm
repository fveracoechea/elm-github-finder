module Routing exposing (Route(..), href, isActiveLink, parseUrlToRoute, routeToPieces, routeToString)

import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), (<?>))
import Url.Parser.Query as Query


type Route
    = Home
    | Search (Maybe String) (Maybe String)
    | Favorites
    | ProfileDetail String


routeParser : Parser.Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Search <|
            Parser.s "search"
                <?> Query.string "query"
                <?> Query.string "entity"
        , Parser.map Favorites <| Parser.s "favorites"
        , Parser.map ProfileDetail <| Parser.s "profiles" </> Parser.string
        ]


parseUrlToRoute : Url -> Route
parseUrlToRoute url =
    Maybe.withDefault Home (Parser.parse routeParser url)


getParam : String -> Maybe String -> List Builder.QueryParameter
getParam name param =
    param
        |> Maybe.map
            (\value ->
                [ Builder.string name value ]
            )
        |> Maybe.withDefault []


routeToString : Route -> String
routeToString page =
    case page of
        Search maybeQuery maybeEntity ->
            let
                query =
                    getParam "query" maybeQuery

                entity =
                    maybeEntity
                        |> Maybe.withDefault "profile"
                        |> Just
                        |> getParam "entity"
            in
            Builder.absolute [ "search" ] (List.append query entity)

        _ ->
            Builder.absolute (routeToPieces page) []


routeToPieces : Route -> List String
routeToPieces page =
    case page of
        Home ->
            []

        Search _ _ ->
            [ "search" ]

        Favorites ->
            [ "favorites" ]

        ProfileDetail username ->
            [ "profiles", username ]


isActiveLink : Route -> Route -> Attribute msg
isActiveLink linkRoute currentRoute =
    let
        linkPath =
            routeToPieces linkRoute
                |> List.head
                |> Maybe.withDefault "/"

        currentPath =
            routeToPieces currentRoute
                |> List.head
                |> Maybe.withDefault "/"
    in
    if linkPath == currentPath then
        Attr.class "active"

    else
        Attr.class ""


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)
