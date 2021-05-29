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
            | SetStateRight Int
            | SetStateWrong Int

update : Msg -> Model -> Model
update msg model = 
    case msg of 
        SubmitPrediction -> 
            {   predictionList = model.predictionList ++ [{name = model.formInput, state = Unknown}]
            ,   formInput = ""  
            }

        PredictionInput input -> 
            { model | formInput = input }

        SetStateRight _ -> model

        SetStateWrong _ -> model

-- VIEW
view : Model -> Html Msg
view model =

    div []
        [ input [ placeholder "Prediction", value model.formInput, onInput PredictionInput] [],
            button [onClick SubmitPrediction, value "Bet"] [text "BET"],
            div [][createListItem {name = "Test", state = Unknown}],
            div [class "prediction-list"](createList model)
        ]         

createList: Model -> List (Html Msg)
createList model =
    List.map createListItem model.predictionList

createListItem: Prediction -> Html Msg
createListItem pred =
    p [](createListContent pred)

createListContent: Prediction -> List (Html Msg)
createListContent pred =
    (case pred.state of
       Unknown -> [text pred.name,text "⚫"]

       Right -> [text pred.name,text "🟢"]

       Wrong -> [text pred.name,text "🔴"]
     ) ++ [button [][text "Right"], button [][text "Wrong"]]