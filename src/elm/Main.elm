module Main exposing (..)

import Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Html.Events.Extra exposing (targetValueMaybe)


-- APP


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- MODEL


model : Model
model =
    { heritors = [] }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddHeritor value ->
            case heritorFromString <| valueWithDefault value "" of
                Just heritor ->
                    { model | heritors = model.heritors ++ [ { heritor = heritor, count = 1 } ] }

                Nothing ->
                    model

        RemoveHeritor heritor ->
            let
                heritors =
                    List.filter ((/=) heritor) model.heritors
            in
                { model | heritors = heritors }

        IncrementHeritor state ->
            changeCountFor state (incrementCount <| availableCountFor state) model

        DecrementHeritor state ->
            changeCountFor state decrementCount model


changeCountFor : HeritorState -> (Int -> Int) -> Model -> Model
changeCountFor { heritor, count } changeCount model =
    let
        incrementState state =
            if state.heritor == heritor then
                { state | count = changeCount state.count }
            else
                state

        heritors =
            List.map incrementState model.heritors
    in
        { model | heritors = heritors }


incrementCount : Int -> Int -> Int
incrementCount availableCount currentCount =
    if currentCount >= availableCount then
        availableCount
    else
        currentCount + 1


decrementCount : Int -> Int
decrementCount currentCount =
    if currentCount <= 1 then
        1
    else
        currentCount - 1


valueWithDefault : Maybe a -> a -> a
valueWithDefault maybeSomething default =
    case maybeSomething of
        Just value ->
            value

        Nothing ->
            default



-- VIEW


view : Model -> Html Msg
view model =
    section [ class "section has-text-right" ]
        [ div [ class "container" ]
            [ div [ class "box" ]
                [ h1 [ class "title has-text-centered" ] [ text "حساب الميراث" ] ]
            , div [ class "box" ]
                [ heritorsView model
                , addHeritorSelect model
                ]
            ]
        ]


heritorsView : Model -> Html Msg
heritorsView model =
    div [] <| List.map heritorView model.heritors


heritorView : HeritorState -> Html Msg
heritorView heritorState =
    div []
        [ i [ class "fa fa-times", onClick <| RemoveHeritor heritorState ] []
        , counter heritorState
        , text <| heritorToString heritorState.heritor
        ]


counter : HeritorState -> Html Msg
counter heritorState =
    let
        { heritor, count } =
            heritorState
    in
        div []
            [ i [ class "fa fa-plus", onClick <| IncrementHeritor heritorState ] []
            , text <| toString count
            , i [ class "fa fa-minus", onClick <| DecrementHeritor heritorState ] []
            ]


availableCountFor : HeritorState -> Int
availableCountFor heritorState =
    List.filter (.heritor >> (==) heritorState.heritor) heritors
        |> List.head
        |> flip valueWithDefault heritorState
        |> .count


addHeritorSelect : Model -> Html Msg
addHeritorSelect model =
    let
        heritorOptions =
            option [ value "", disabled True, selected True ] [ text "اختر وارثا" ]
                :: List.map (.heritor >> heritorToString >> heritorOption) (unselectedHeritors model)
    in
        select [ on "change" (Json.Decode.map AddHeritor targetValueMaybe) ] heritorOptions


heritorOption : String -> Html Msg
heritorOption heritor =
    option [ value heritor ] [ text heritor ]


unselectedHeritors : Model -> List HeritorState
unselectedHeritors model =
    let
        selectedHeritors =
            List.map .heritor model.heritors
    in
        List.filter (.heritor >> flip List.member selectedHeritors >> not) heritors
