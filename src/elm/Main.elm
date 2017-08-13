module Main exposing (..)

import Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import HeritorManagement.View
import HeritorManagement.State


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
            HeritorManagement.State.addHeritor model value

        RemoveHeritor heritor ->
            HeritorManagement.State.removeHeritor model heritor

        IncrementHeritor state ->
            HeritorManagement.State.incrementHeritor model state

        DecrementHeritor state ->
            HeritorManagement.State.decrementHeritor model state



-- VIEW


view : Model -> Html Msg
view model =
    section [ class "section has-text-right" ]
        [ div [ class "container" ]
            [ div [ class "box" ]
                [ h1 [ class "title has-text-centered" ] [ text "حساب الميراث" ] ]
            , HeritorManagement.View.view model
            ]
        ]
