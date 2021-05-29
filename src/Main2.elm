module Main exposing (main)

import Browser exposing (sandbox)
import Html exposing (button, text, div, Html)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

type Msg = Increment

type alias Model = Int

view model = 
    div [class "text-center"] 
        [ div [] [ text ("BET " ++ (String.fromInt model))] 
        , button 
            [ class "btn btn-primary", onClick Increment]
            [ text "+"]
        ]

update msg model =
    case msg of
        Increment ->
            model + 1

main = sandbox 
            { init = 0, view = view, update = update}