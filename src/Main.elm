module Main exposing (..)


import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Utils

-- MAIN

main = Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Model = {predictionList: List Prediction, formInput: String, predictionsCreated: Int}

type alias Prediction = { id: Int, name: String, state: PredictionState}

type PredictionState = Unknown | Right | Wrong

init: Model
init = Model [] "" 0

-- UPDATE

type Msg = SubmitPrediction 
            | PredictionInput String
            | SetState Int PredictionState

update : Msg -> Model -> Model
update msg model = 
    case msg of 
        SubmitPrediction -> 
            {   predictionList = model.predictionList ++ [{id = model.predictionsCreated, name = model.formInput, state = Unknown}]
            ,   formInput = "" , predictionsCreated = model.predictionsCreated + 1
            }

        PredictionInput input -> 
            { model | formInput = input }

        SetState id state  -> setState id state model


setState: Int -> PredictionState -> Model -> Model 
setState idx state model =
    {   model
        | predictionList = ( Utils.findAndUpdate (\pred -> pred.id == idx) (\pred -> {pred|state = state}) model.predictionList  )   
    }
    

-- VIEW
view : Model -> Html Msg
view model =

    div []
        [ input [ placeholder "Prediction", value model.formInput, onInput PredictionInput] [],
            button [onClick SubmitPrediction, value "Bet"] [text "BET"],
            div [][createListItem {id = 0, name = "Test", state = Unknown}],
            div [class "prediction-list"](createList model)
        ]         

createList: Model -> List (Html Msg)
createList model =
    List.map createListItem model.predictionList

createListItem: Prediction -> Html Msg
createListItem pred =
    p [id ("prediction-" ++ (String.fromInt pred.id))](createListContent pred)

createListContent: Prediction -> List (Html Msg)
createListContent pred =
    (case pred.state of
       Unknown -> [text pred.name,text "⚫"]

       Right -> [text pred.name,text "🟢"]

       Wrong -> [text pred.name,text "🔴"]
     ) ++ [button [onClick (SetState pred.id Right)][text "Right"], button [onClick (SetState pred.id Wrong)][text "Wrong"]]