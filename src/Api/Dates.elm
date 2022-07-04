module Api.Dates exposing (..)

import Date as Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (..)
import Time


defaultDate : Date
defaultDate =
    0
        |> Time.millisToPosix
        |> Date.fromPosix Time.utc


dateFromString : String -> Result String Date.Date
dateFromString stringDate =
    stringDate
        |> String.split "T"
        |> List.head
        |> Maybe.withDefault "invalid-date"
        |> Date.fromIsoString


renderDate : String -> Html msg
renderDate stringDate =
    case
        dateFromString stringDate
            |> Result.map (Date.format "MMMM y")
    of
        Ok date ->
            span
                [ title "created on", style "font-size" ".8rem" ]
                [ i
                    [ class "bi bi-calendar2-week-fill text-primary me-1"
                    , style "font-size" "1.2rem"
                    ]
                    []
                , text date
                ]

        Err _ ->
            text ""


getDateWidthDefault : String -> Date
getDateWidthDefault date =
    date
        |> dateFromString
        |> Result.withDefault defaultDate
