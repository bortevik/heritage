module HeritorManagement.View exposing (view)

import Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events.Extra exposing (targetValueMaybe)
import Json.Decode


view : Model -> Html Msg
view model =
    div [ class "box heritors-list" ]
        [ heritorsView model
        , heritorSelect model
        ]


heritorsView : Model -> Html Msg
heritorsView model =
    let
        selectedHeritors =
            List.filter (.selected >> (==) Selected) model.heritors
    in
      div [] <| List.map heritorView selectedHeritors


heritorView : HeritorState -> Html Msg
heritorView heritorState =
    div [ class "heritor-row" ]
        [ div [ class "heritor-label" ] [ text <| heritorToString heritorState.heritor ]
        , counter heritorState
        , i [ class "remove-icon fa fa-times", onClick <| RemoveHeritor heritorState ] []
        ]


counter : HeritorState -> Html Msg
counter heritorState =
    let
        { heritor, count } =
            heritorState
    in
        div [ class "counter" ]
            [ decrementButton heritorState
            , span [ class "heritor-count" ] [ text <| toString count ]
            , incrementButton heritorState
            ]


decrementButton : HeritorState -> Html Msg
decrementButton state =
    let
        disabled =
            if state.count <= 1 then
                " disabled"
            else
                ""

        classNames =
            "decrement-icon fa fa-minus" ++ disabled
    in
        i [ class classNames, onClick <| DecrementHeritor state ] []


incrementButton : HeritorState -> Html Msg
incrementButton state =
    let
        disabled =
            if state.count >= state.availableCount then
                " disabled"
            else
                ""

        classNames =
            "increment-icon fa fa-plus" ++ disabled
    in
        i [ class classNames, onClick <| IncrementHeritor state ] []


heritorSelect : Model -> Html Msg
heritorSelect model =
    let
        heritorOptions =
            option [ value "", disabled True, selected True ] [ text "اختر وارثا" ]
                :: List.map (.heritor >> heritorToString >> heritorOption) (unselectedHeritors model)
    in
        select [ on "change" (Json.Decode.map SelectHeritor targetValueMaybe) ] heritorOptions


heritorOption : String -> Html Msg
heritorOption heritor =
    option [ value heritor ] [ text heritor ]


unselectedHeritors : Model -> List HeritorState
unselectedHeritors model =
    List.filter (.selected >> ( == ) Unselected) model.heritors
