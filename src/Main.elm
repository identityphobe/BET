module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

-- MAIN

main = Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Model = {predictionList: List Prediction, formInput: String}

type alias Prediction = { name: String, state: PredictionState}

type PredictionState = Unknown | Right | Wrong

init: Model
init = Model [] ""

-- UPDATE

type Msg = SubmitPrediction 
            | PredictionInput String

update : Msg -> Model -> Model
update msg model = 
    case msg of 
        SubmitPrediction -> 
            {   predictionList = model.predictionList ++ [{name = model.formInput, state = Unknown}]
            ,   formInput = ""  
            }

        PredictionInput input -> 
            { model | formInput = input }

-- VIEW
view : Model -> Html Msg
view model =

    div []
        [ input [ placeholder "Prediction", value model.formInput, onInput PredictionInput] [],
            button [onClick SubmitPrediction, value "Bet"] [text "BET"],

          if not (List.isEmpty model.predictionList) then 
            ol [] (createList model)
          else 
            text ""
        ]         

createList: Model -> List (Html Msg)
createList model =
    List.map (\pred -> li [][text pred.name]) model.predictionList

    