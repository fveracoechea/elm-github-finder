module Page.Home exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Types exposing (..)


renderItem : String -> String -> Html msg
renderItem content icon =
    if String.isEmpty content then
        text ""

    else
        p []
            [ i [ class icon, style "padding-right" "5px" ] []
            , span [ class "text-muted" ] [ text content ]
            ]


renderMaybeItem : Maybe String -> String -> Html msg
renderMaybeItem maybeItem icon =
    case maybeItem of
        Just item ->
            renderItem item icon

        Nothing ->
            text ""


renderUserHeading : Profile -> Html msg
renderUserHeading profile =
    div [ class "row" ]
        [ div [ class "col-4" ]
            [ div []
                [ img [ src profile.avatar_url, class "img-thumbnail img-fluid" ] []
                ]
            ]
        , div [ class "col-8" ]
            [ h3 [ class "text-primary" ] [ text (Maybe.withDefault profile.location profile.name) ]
            , p [] [ a [ class "text-muted", href profile.html_url ] [ text profile.login ] ]
            , renderItem profile.location "bi bi-geo-alt-fill"
            , renderMaybeItem profile.bio "bi bi-bookmark-fill"
            , renderMaybeItem profile.blog "bi bi-globe"
            , renderItem ("Followers: " ++ String.fromInt profile.followers) "bi bi-people-fill"
            , renderItem ("Following: " ++ String.fromInt profile.following) "bi bi-person-heart"
            ]
        ]


view : ProfileState -> Html msg
view state =
    div [ class "pt-5" ]
        [ case state of
            Fullfilled profile ->
                renderUserHeading profile

            _ ->
                div [] [ h4 [ class "text-muted" ] [ text "Search for a github profile" ] ]
        ]
