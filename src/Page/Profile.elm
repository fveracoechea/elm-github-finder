module Page.Profile exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Types exposing (..)


renderItem : String -> String -> Html msg
renderItem content icon =
    if String.isEmpty content then
        text ""

    else
        p []
            [ i [ class icon, style "padding-right" "5px", style "color" "#fff" ] []
            , span [ style "color" "#c4c4c4" ] [ text content ]
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
    div [ class "p-3 mb-4 bg-dark rounded-3 text-white" ]
        [ div [ class "row" ]
            [ div [ class "col-4" ]
                [ div []
                    [ img [ src profile.avatar_url, class "rounded-1 img-fluid" ] []
                    ]
                ]
            , div [ class "col-8 d-flex flex-column justify-content-between" ]
                [ div []
                    [ h2 [ class "border-bottom mb-4", style "color" "#c4c4c4" ] [ text (Maybe.withDefault profile.location profile.name) ]
                    , renderItem profile.location "bi bi-geo-alt-fill"
                    , renderMaybeItem profile.blog "bi bi-globe"
                    , renderItem ("Followers: " ++ String.fromInt profile.followers) "bi bi-people-fill"
                    , renderItem ("Following: " ++ String.fromInt profile.following) "bi bi-person-heart"
                    , renderMaybeItem profile.bio "bi bi-bookmark-fill"
                    ]
                , div [ class "d-flex flex-column align-items-end" ]
                    [ a [ href profile.html_url, class "btn btn-link" ] [ text profile.login ]
                    ]
                ]
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
