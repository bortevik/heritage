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
        , addHeritorSelect model
        ]


heritorsView : Model -> Html Msg
heritorsView model =
    div [] <| List.map heritorView model.heritors


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
            [ i [ class "decrement-icon fa fa-minus", onClick <| DecrementHeritor heritorState ] []
            , span [ class "heritor-count" ] [ text <| toString count ]
            , i [ class "increment-icon fa fa-plus", onClick <| IncrementHeritor heritorState ] []
            ]


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
