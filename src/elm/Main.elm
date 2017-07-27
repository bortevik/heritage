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
                    { model | heritors = model.heritors ++ [ heritor ] }

                Nothing ->
                    model

        RemoveHeritor heritor ->
            let
                heritors =
                    List.filter ((/=) heritor) model.heritors
            in
                { model | heritors = heritors }


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


heritorView : Heritor -> Html Msg
heritorView heritor =
    div []
        [ i [ class "fa fa-times", onClick <| RemoveHeritor heritor ] []
        , text <| heritorToString heritor
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


unselectedHeritors : Model -> List { heritor : Heritor, count : Int }
unselectedHeritors model =
    List.filter (.heritor >> flip List.member model.heritors >> not) heritors
