module HeritorManagement.State
    exposing
        ( addHeritor
        , removeHeritor
        , incrementHeritor
        , decrementHeritor
        )

import Types exposing (..)
import Utils exposing (valueWithDefault)


addHeritor : Model -> Maybe String -> Model
addHeritor model maybeHeritorString =
    case heritorFromString <| valueWithDefault maybeHeritorString "" of
        Just heritor ->
            { model | heritors = model.heritors ++ [ { heritor = heritor, count = 1 } ] }

        Nothing ->
            model


removeHeritor : Model -> HeritorState -> Model
removeHeritor model heritor =
    let
        heritors =
            List.filter ((/=) heritor) model.heritors
    in
        { model | heritors = heritors }


setHeritorsCount : Model -> String -> Model
setHeritorsCount model value =
    case String.toInt value of
        Ok newCount ->
            model

        Err _ ->
            model


incrementHeritor : Model -> HeritorState -> Model
incrementHeritor model state =
    changeCountFor state (incrementCount <| availableCountFor state) model


decrementHeritor : Model -> HeritorState -> Model
decrementHeritor model state =
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


availableCountFor : HeritorState -> Int
availableCountFor heritorState =
    List.filter (.heritor >> (==) heritorState.heritor) heritors
        |> List.head
        |> flip valueWithDefault heritorState
        |> .count
