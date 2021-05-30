module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Encode as Encode
import Ports
import Utils



-- MAIN
-- main: Program flags Model Msg


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL--


type alias Model =
    { predictionList : List Prediction, formInput : String, predictionsCreated : Int }


type alias Prediction =
    { id : Int, name : String, state : PredictionState }


type PredictionState
    = Unknown
    | Right
    | Wrong


init : () -> ( Model, Cmd Msg )
init _ =
    ( { predictionList = [], formInput = "", predictionsCreated = 0 }, Cmd.none )


savePredictions : List Prediction -> Cmd Msg
savePredictions predictions =
    Encode.list predictionEncoder predictions |> Encode.encode 0 |> Ports.storePredictions


predictionEncoder : Prediction -> Encode.Value
predictionEncoder prediction =
    Encode.object
        [ ( "id", Encode.int prediction.id )
        , ( "name", Encode.string prediction.name )
        , ( "state", predictionStateEncoder prediction.state )
        ]


predictionStateEncoder : PredictionState -> Encode.Value
predictionStateEncoder state =
    case state of
        Unknown ->
            Encode.string "Unknown"

        Right ->
            Encode.string "Right"

        Wrong ->
            Encode.string "Wrong"



-- UPDATE


type Msg
    = SubmitPrediction
    | PredictionInput String
    | SetState Int PredictionState



-- TODO: Remove repetition when trying to send the new model to local storage. Possible solution: sequentially run Cmd


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmitPrediction ->
            ( createModelAfterSubmission model
            , savePredictions (createModelAfterSubmission model).predictionList
            )

        PredictionInput input ->
            ( { model | formInput = input }, savePredictions (createModelAfterSubmission { model | formInput = input }).predictionList )

        SetState id state ->
            ( setState id state model, savePredictions (setState id state model).predictionList )


createModelAfterSubmission : Model -> Model
createModelAfterSubmission model =
    { predictionList = model.predictionList ++ [ { id = model.predictionsCreated, name = model.formInput, state = Unknown } ]
    , formInput = ""
    , predictionsCreated = model.predictionsCreated + 1
    }


setState : Int -> PredictionState -> Model -> Model
setState idx state model =
    { model
        | predictionList = Utils.findAndUpdate (\pred -> pred.id == idx) (\pred -> { pred | state = state }) model.predictionList
    }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Prediction", value model.formInput, onInput PredictionInput ] []
        , button [ onClick SubmitPrediction, value "Bet" ] [ text "BET" ]
        , div [] [ createListItem { id = 0, name = "Test", state = Unknown } ]
        , div [ class "prediction-list" ] (createList model)
        ]


createList : Model -> List (Html Msg)
createList model =
    List.map createListItem model.predictionList


createListItem : Prediction -> Html Msg
createListItem pred =
    p [ id ("prediction-" ++ String.fromInt pred.id) ] (createListContent pred)


createListContent : Prediction -> List (Html Msg)
createListContent pred =
    (case pred.state of
        Unknown ->
            [ text pred.name, text "⚫" ]

        Right ->
            [ text pred.name, text "🟢" ]

        Wrong ->
            [ text pred.name, text "🔴" ]
    )
        ++ [ button [ onClick (SetState pred.id Right) ] [ text "Right" ], button [ onClick (SetState pred.id Wrong) ] [ text "Wrong" ] ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
