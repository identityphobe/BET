module Main exposing (..)

-- import Debug

import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes as Attributes exposing (class, id, placeholder, required, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder, Error(..), decodeString, field, index, int, map4, string)
import Json.Encode as Encode
import Ports
import String exposing (isEmpty)
import Task
import Utils



-- MAIN


main : Program (Maybe String) Model Msg
main =
    Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL--


type alias Model =
    { predictionList : List Prediction, formInput : String, rangeInput : Int, inputError : String, predictionsCreated : Int }


type alias Prediction =
    { id : Int, name : String, state : PredictionState, difficulty : ( Difficulty, Difficulty ) }


type PredictionState
    = Unknown
    | Right
    | Wrong


type Difficulty
    = DifficultyUnknown
    | Easy
    | Doable
    | Difficult
    | VeryDifficult
    | Impossible


init : Maybe String -> ( Model, Cmd Msg )
init flags =
    case flags of
        Just predictionsJson ->
            -- TODO: build a full decoder that encodes predicitonsCreated
            ( { predictionList = decodePredictionList predictionsJson, formInput = "", rangeInput = 3, inputError = "", predictionsCreated = 0 }, Cmd.none )

        Nothing ->
            ( { predictionList = [], formInput = "", rangeInput = 3, inputError = "", predictionsCreated = 0 }, Cmd.none )


savePredictions : List Prediction -> Cmd Msg
savePredictions predictions =
    Encode.list encodePredictions predictions |> Encode.encode 0 |> Ports.storePredictions


encodePredictions : Prediction -> Encode.Value
encodePredictions prediction =
    Encode.object
        [ ( "id", Encode.int prediction.id )
        , ( "name", Encode.string prediction.name )
        , ( "state", encodePredictionState prediction.state )
        , ( "difficulty", encodeDifficulty prediction.difficulty )
        ]



-- this encodes the tuple of Difficulty within model


encodeDifficulty : ( Difficulty, Difficulty ) -> Encode.Value
encodeDifficulty ( a, b ) =
    Encode.list difficultyEncoder [ a, b ]



-- this encodes a Difficulty variant into string


difficultyEncoder : Difficulty -> Encode.Value
difficultyEncoder difficulty =
    case difficulty of
        DifficultyUnknown ->
            Encode.string "Unknown"

        Easy ->
            Encode.string "Easy"

        Doable ->
            Encode.string "Doable"

        Difficult ->
            Encode.string "Difficult"

        VeryDifficult ->
            Encode.string "VeryDifficult"

        Impossible ->
            Encode.string "Impossible"


encodePredictionState : PredictionState -> Encode.Value
encodePredictionState state =
    case state of
        Unknown ->
            Encode.string "Unknown"

        Right ->
            Encode.string "Right"

        Wrong ->
            Encode.string "Wrong"



-- decoder for prediction list


decodePredictionList : String -> List Prediction
decodePredictionList predictionsJson =
    let
        decodedJson =
            decodeString predictionListDecoder predictionsJson
    in
    case decodedJson of
        Ok decodedPredictionList ->
            decodedPredictionList

        -- TODO: Error handling
        Err _ ->
            []


predictionListDecoder : Decoder (List Prediction)
predictionListDecoder =
    Decode.list predictionDecoder


predictionDecoder : Decoder Prediction
predictionDecoder =
    map4 Prediction (field "id" int) (field "name" string) (field "state" predictionStateDecoder) (field "difficulty" <| difficultyDecoder)


difficultyDecoder : Decoder ( Difficulty, Difficulty )
difficultyDecoder =
    index 0 (Decode.string |> Decode.andThen difficultyFromString)
        |> Decode.andThen
            (\firstVal ->
                index 1 (Decode.string |> Decode.andThen difficultyFromString)
                    |> Decode.andThen (\secondVal -> Decode.succeed ( firstVal, secondVal ))
            )


difficultyFromString : String -> Decoder Difficulty
difficultyFromString string =
    case string of
        "Unknown" ->
            Decode.succeed DifficultyUnknown

        "Easy" ->
            Decode.succeed Easy

        "Doable" ->
            Decode.succeed Doable

        "Difficult" ->
            Decode.succeed Difficult

        "VeryDifficult" ->
            Decode.succeed VeryDifficult

        "Impossible" ->
            Decode.succeed Impossible

        _ ->
            Decode.fail ("Invalid difficulty: " ++ string)


predictionStateDecoder : Decoder PredictionState
predictionStateDecoder =
    Decode.string |> Decode.andThen predictionStateFromString


predictionStateFromString : String -> Decoder PredictionState
predictionStateFromString string =
    case string of
        "Unknown" ->
            Decode.succeed Unknown

        "Right" ->
            Decode.succeed Right

        "Wrong" ->
            Decode.succeed Wrong

        _ ->
            Decode.fail ("Invalid prediction state: " ++ string)


decodeStoredPredictions : String -> String
decodeStoredPredictions predictionsJson =
    "Received by elm: " ++ predictionsJson



-- UPDATE
-- TODO: The inputs Msg are best named as verbs


type Msg
    = SubmitPrediction
    | PredictionInput String
    | SetState Int PredictionState
    | RangeInput String
    | EmptyInput



-- TODO: Remove repetition when trying to send the new model to local storage. Possible solution: sequentially run Cmd
--       Maybe, return a tuple that contains the model-generating function and the savePredictions function that uses that same function


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmitPrediction ->
            validateActionInput model

        EmptyInput ->
            ( model, Cmd.none )

        PredictionInput input ->
            ( { model | formInput = input }, savePredictions model.predictionList )

        RangeInput input ->
            ( { model | rangeInput = String.toInt input |> Maybe.withDefault 0 }, savePredictions model.predictionList )

        SetState id state ->
            ( setState id state model, savePredictions (setState id state model).predictionList )


createModelAfterSubmission : Model -> Model
createModelAfterSubmission model =
    let
        expectedDifficulty =
            convertNumToDifficulty model.rangeInput
    in
    { predictionList = model.predictionList ++ [ { id = model.predictionsCreated, name = model.formInput, state = Unknown, difficulty = ( expectedDifficulty, DifficultyUnknown ) } ]
    , formInput = ""
    , rangeInput = 3
    , inputError = ""
    , predictionsCreated = model.predictionsCreated + 1
    }


validateActionInput : Model -> ( Model, Cmd Msg )
validateActionInput model =
    if isEmpty model.formInput then
        -- TODO: handle error if Dom.focus can't find the id of the element to focus
        ( { model | inputError = "Are you afraid of nothing?" }, Task.attempt (\_ -> EmptyInput) (Dom.focus "action-input") )

    else
        ( createModelAfterSubmission model
        , savePredictions (createModelAfterSubmission model).predictionList
        )


setState : Int -> PredictionState -> Model -> Model
setState idx state model =
    { model
        | predictionList = Utils.findAndUpdate (\pred -> pred.id == idx) (\pred -> { pred | state = state }) model.predictionList
    }


convertNumToDifficulty : Int -> Difficulty
convertNumToDifficulty num =
    case num of
        1 ->
            Easy

        2 ->
            Doable

        3 ->
            Difficult

        4 ->
            VeryDifficult

        5 ->
            Impossible

        _ ->
            DifficultyUnknown



-- VIEW


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


view : Model -> Document Msg
view model =
    { title = "BET"
    , body =
        [ div [ id "app-container" ]
            [ inputView model
            , predictionListView model

            -- , div [ class "prediction-list" ] (createList model)
            ]
        ]
    }


inputView : Model -> Html Msg
inputView model =
    let
        defaultInputContainer =
            [ input [ id "action-input", placeholder "I think...", value model.formInput, onInput PredictionInput ] []
            , p [] [ text "...is ", span [ class <| setDifficultyClass model.rangeInput ] [ strong [] [ text <| setDynDifficultyText model.rangeInput ] ] ]
            , input [ type_ "range", Attributes.min "1", Attributes.max "5", value <| String.fromInt model.rangeInput, onInput RangeInput ] []
            , button [ id "submitButton", onClick SubmitPrediction ] [ text "Test It" ]

            -- if not isEmpty(model.inputError) then
            --     p [][text "test"]
            ]
    in
    div [ class "input-container" ]
        (if isEmpty model.inputError then
            defaultInputContainer

         else
            List.append defaultInputContainer <| [ p [ class "input-error" ] [ text model.inputError ] ]
        )



-- [ input [ class "action-input", placeholder "I think...", value model.formInput, onInput PredictionInput ] []
-- , p [] [ text "...is ", span [ class <| setDifficultyClass model.rangeInput ] [ strong [] [ text <| setDynDifficultyText model.rangeInput ] ] ]
-- , input [ type_ "range", Attributes.min "1", Attributes.max "5", value <| String.fromInt model.rangeInput, onInput RangeInput ] []
-- , button [ id "submitButton", onClick SubmitPrediction ] [ text "Test It" ]
-- -- if not isEmpty(model.inputError) then
-- --     p [][text "test"]
-- ]


setDynDifficultyText : Int -> String
setDynDifficultyText difficulty =
    case difficulty of
        1 ->
            "EASY"

        2 ->
            "DOABLE"

        3 ->
            "DIFFICULT"

        4 ->
            "VERY DIFFICULT"

        5 ->
            "IMPOSSIBLE"

        _ ->
            "???"


setDifficultyClass : Int -> String
setDifficultyClass difficulty =
    case difficulty of
        1 ->
            "easy"

        2 ->
            "doable"

        3 ->
            "difficult"

        4 ->
            "very-difficult"

        5 ->
            "impossible"

        _ ->
            "unknown"


predictionListView : Model -> Html Msg
predictionListView model =
    div [ id "prediction-list-container" ] (createList model)


createList : Model -> List (Html Msg)
createList model =
    List.map createListItem model.predictionList


createListItem : Prediction -> Html Msg
createListItem pred =
    div [ id ("prediction-" ++ String.fromInt pred.id), class "prediction-container" ] (createListContent pred)


createListContent : Prediction -> List (Html Msg)
createListContent pred =
    [ p [ class "prediction-name" ] [ text pred.name ] ]



-- case pred.state of
--     Unknown ->
--         [ p [] [ text pred.name ] ]
--     Right ->
--         [ text pred.name, text "🟢" ]
--     Wrong ->
--         [ text pred.name, text "🔴" ]
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
