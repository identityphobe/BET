module Main exposing (..)

-- import Debug

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Dict
import Html exposing (..)
import Html.Attributes as Attributes exposing (class, href, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder, Error(..), decodeString, field, index, int, map4, string)
import Json.Encode as Encode
import List
import Platform exposing (Router)
import Ports
import String exposing (isEmpty)
import Task
import Tuple
import Url
import Url.Parser as UrlParser exposing ((</>), (<?>), Parser)
import Url.Parser.Query as Query
import Utils



-- MAIN


main : Program (Maybe String) Model Msg
main =
    Browser.application { init = init, update = update, view = view, subscriptions = subscriptions, onUrlChange = UrlChanged, onUrlRequest = LinkClicked }



-- MODEL--


type alias Model =
    { predictionList : List Prediction, formInput : String, rangeInput : Int, inputError : String, predictionsCreated : Int, key : Nav.Key, route : Maybe Route }


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


type Route
    = NewPrediction
    | ReportPrediction (Maybe Int)


routeParser : Parser (Route -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map NewPrediction (UrlParser.s "new")
        , UrlParser.map ReportPrediction (UrlParser.s "report" <?> Query.int "id")
        ]


init : Maybe String -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    case flags of
        Just predictionsJson ->
            -- TODO: build a full decoder that encodes predicitonsCreated
            ( { predictionList = decodePredictionList predictionsJson, formInput = "", rangeInput = 3, inputError = "", predictionsCreated = 0, key = key, route = UrlParser.parse routeParser url }, Cmd.none )

        Nothing ->
            ( { predictionList = [], formInput = "", rangeInput = 3, inputError = "", predictionsCreated = 0, key = key, route = UrlParser.parse routeParser url }, Cmd.none )


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
    | SubmitReport Int
    | PredictionInput String
    | SetState Int PredictionState
    | RangeInput String
    | EmptyInput
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | OpenReportPage Int
    | SavePredictions



-- TODO: Remove repetition when trying to send the new model to local storage. Possible solution: sequentially run Cmd
--       Maybe, return a tuple that contains the model-generating function and the savePredictions function that uses that same function


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmitPrediction ->
            validateActionInput createModelAfterSubmission model

        SubmitReport input ->
            update SavePredictions <| updateModelAfterReport input model

        SavePredictions ->
            ( model, savePredictions model.predictionList )

        EmptyInput ->
            ( model, Cmd.none )

        PredictionInput input ->
            ( { model | formInput = input }, savePredictions model.predictionList )

        RangeInput input ->
            ( { model | rangeInput = String.toInt input |> Maybe.withDefault 0 }, savePredictions model.predictionList )

        SetState id state ->
            ( setState id state model, savePredictions (setState id state model).predictionList )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | route = UrlParser.parse routeParser url }
            , Cmd.none
            )

        OpenReportPage id ->
            ( model, Nav.pushUrl model.key ("report?id=" ++ String.fromInt id) )


createModelAfterSubmission : Model -> Model
createModelAfterSubmission model =
    let
        expectedDifficulty =
            difficultyFromInt model.rangeInput
    in
    { model
        | predictionList = model.predictionList ++ [ { id = getMaxId model.predictionList + 1, name = model.formInput, state = Unknown, difficulty = ( expectedDifficulty, DifficultyUnknown ) } ]
        , formInput = ""
        , rangeInput = 3
        , inputError = ""

        -- consider storing this as JSON or remove it altogether
        , predictionsCreated = getMaxId model.predictionList + 1
    }


getMaxId : List Prediction -> Int
getMaxId list =
    Maybe.withDefault
        0
    <|
        List.head <|
            List.reverse <|
                List.sort <|
                    List.map (\pred -> pred.id) list


updateModelAfterReport : Int -> Model -> Model
updateModelAfterReport idx model =
    let
        reportedDifficulty =
            difficultyFromInt model.rangeInput

        updatedPredictionList =
            Utils.findAndUpdate (\pred -> pred.id == idx) (\pred -> { pred | difficulty = ( Tuple.first pred.difficulty, reportedDifficulty ) }) model.predictionList
    in
    { model | predictionList = updatedPredictionList }


validateActionInput : (Model -> Model) -> Model -> ( Model, Cmd Msg )
validateActionInput func model =
    if isEmpty model.formInput then
        -- TODO: handle error if Dom.focus can't find the id of the element to focus
        ( { model | inputError = "Are you afraid of nothing?" }, Task.attempt (\_ -> EmptyInput) (Dom.focus "action-input") )

    else
        ( func model
        , savePredictions (func model).predictionList
        )


setState : Int -> PredictionState -> Model -> Model
setState idx state model =
    { model
        | predictionList = Utils.findAndUpdate (\pred -> pred.id == idx) (\pred -> { pred | state = state }) model.predictionList
    }


difficultyFromInt : Int -> Difficulty
difficultyFromInt num =
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
    -- (Debug.log <|
    --     Url.toString model.route
    -- )
    { title = "BET"
    , body =
        case model.route of
            Just NewPrediction ->
                [ div [ id "app-container" ]
                    [ li [] [ a [ href "/" ] [ text "/" ] ]
                    , inputView model
                    ]
                ]

            Just (ReportPrediction idx) ->
                case idx of
                    Just num ->
                        [ div [ id "app-container" ]
                            [ reportView
                                num
                                model
                            ]
                        ]

                    Nothing ->
                        [ div [ id "app-container" ]
                            [ text "Something's wrong with your report url. Make sure the id is there and a proper integer." ]
                        ]

            Nothing ->
                appView inputView model

    -- [ div [ id "app-container" ]
    --     [
    --     , li [] [ a [ href "/list" ] [ text "/list" ] ]
    --     , predictionListView model
    --     ]
    -- ]
    }


appView : (Model -> Html Msg) -> Model -> List (Html Msg)
appView view_comp model =
    [ div [ id "app-container" ]
        [ div [ id "nav-bar" ] [ div [ id "current" ] [ p [] [ text "Home" ] ], div [] [ p [] [ text "List" ] ] ]
        , view_comp model
        ]
    ]


inputView : Model -> Html Msg
inputView model =
    let
        defaultInputContainer =
            [ input [ id "action-input", placeholder "I think...", value model.formInput, onInput PredictionInput ] []
            , p [] [ text "...is ", span [ class <| setDifficultyClass model.rangeInput ] [ strong [] [ text <| difficultyStringFromInt model.rangeInput ] ] ]
            , input [ type_ "range", Attributes.min "1", Attributes.max "5", value <| String.fromInt model.rangeInput, onInput RangeInput ] []
            , button [ id "submitButton", onClick SubmitPrediction ] [ text "Test It" ]
            ]
    in
    div [ class "input-container" ]
        (if isEmpty model.inputError then
            defaultInputContainer

         else
            List.append defaultInputContainer <| [ p [ class "input-error" ] [ text model.inputError ] ]
        )


reportView : Int -> Model -> Html Msg
reportView idx model =
    let
        activityName =
            case Utils.find (\pred -> pred.id == idx) model.predictionList of
                Just foundActivity ->
                    foundActivity.name

                Nothing ->
                    "making sure the report page id"

        defaultInputContainer =
            [ p [] [ text "Previously, I thought ", span [ class "report-activity-name" ] [ text activityName ], text " was ", span [ class <| setDifficultyClass model.rangeInput ] [ strong [] [ text <| difficultyStringFromInt model.rangeInput ] ] ]
            , input [ type_ "range", Attributes.min "1", Attributes.max "5", value <| String.fromInt model.rangeInput, onInput RangeInput ] []
            , button [ id "submitButton", onClick (SubmitReport idx) ] [ text "Report" ]
            ]

        id_exists =
            Utils.find (\pred -> pred.id == idx) model.predictionList
    in
    case id_exists of
        Just _ ->
            div [ class "input-container" ]
                (if isEmpty model.inputError then
                    defaultInputContainer

                 else
                    List.append defaultInputContainer <| [ p [ class "input-error" ] [ text model.inputError ] ]
                )

        Nothing ->
            p [] [ text "That's the wrong id, o' person of importance." ]


difficultyStringFromInt : Int -> String
difficultyStringFromInt difficulty =
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



-- this function is dependent on difficultyStringFromInt
-- maybe I should change this to be independent of that function


stringFromDifficulty : Difficulty -> String
stringFromDifficulty difficulty =
    intFromDifficulty difficulty |> difficultyStringFromInt


intFromDifficulty : Difficulty -> Int
intFromDifficulty difficulty =
    case difficulty of
        DifficultyUnknown ->
            0

        Easy ->
            1

        Doable ->
            2

        Difficult ->
            3

        VeryDifficult ->
            4

        Impossible ->
            5



-- class here refers to CSS class
-- difficulty value corresponds to the int value from range slider


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


createPredictionResultText : Difficulty -> Difficulty -> String
createPredictionResultText expectedDifficulty actualDifficulty =
    if intFromDifficulty expectedDifficulty >= intFromDifficulty actualDifficulty then
        "RIGHT"

    else
        "WRONG"


createPredictionResultSpanEl : Difficulty -> Difficulty -> Html Msg
createPredictionResultSpanEl expectedDifficulty actualDifficulty =
    let
        result =
            createPredictionResultText expectedDifficulty actualDifficulty
    in
    span [ class <| result ] [ text result ]


predictionListView : Model -> Html Msg
predictionListView model =
    div [ id "prediction-list-container" ] (createList model)


createList : Model -> List (Html Msg)
createList model =
    List.map createListItem model.predictionList


createListItem : Prediction -> Html Msg
createListItem pred =
    div [ id ("prediction-" ++ String.fromInt pred.id), class "prediction-container", onClick (OpenReportPage pred.id) ] (createListContent pred)


createListContent : Prediction -> List (Html Msg)
createListContent pred =
    [ p [ class "prediction-name" ] [ text pred.name ], p [ class "match-container" ] [ createMatchTexts pred ] ]


createMatchTexts : Prediction -> Html Msg
createMatchTexts pred =
    let
        ( expectedDifficulty, actualDifficulty ) =
            pred.difficulty
    in
    p [ class "match-versus-text" ] [ createDifficultySpanEl expectedDifficulty, span [] [ text "VS" ], createDifficultySpanEl actualDifficulty, span [] [ text " = " ], text "I was ", createPredictionResultSpanEl actualDifficulty expectedDifficulty ]


createDifficultySpanEl : Difficulty -> Html Msg
createDifficultySpanEl difficulty =
    -- TODO: reduce repetition and clean up css to distinguish difficulty text and difficulty background
    case difficulty of
        DifficultyUnknown ->
            span [ class "unknown match-difficulty-text" ] [ text <| stringFromDifficulty difficulty ]

        Easy ->
            span [ class "easy match-difficulty-text" ] [ text <| stringFromDifficulty difficulty ]

        Doable ->
            span [ class "doable match-difficulty-text" ] [ text <| stringFromDifficulty difficulty ]

        Difficult ->
            span [ class "difficult match-difficulty-text" ] [ text <| stringFromDifficulty difficulty ]

        VeryDifficult ->
            span [ class "very-difficult match-difficulty-text" ] [ text <| stringFromDifficulty difficulty ]

        Impossible ->
            span [ class "impossible match-difficulty-text" ] [ text <| stringFromDifficulty difficulty ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
