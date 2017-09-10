module HeritageCalculation.State exposing (..)

import Types exposing (..)
import Utils.Fraction as Fraction exposing (Fraction)


whole : Fraction
whole =
    ( 1, 1 )


half : Fraction
half =
    ( 1, 2 )


oneThird : Fraction
oneThird =
    ( 1, 3 )


oneForth : Fraction
oneForth =
    ( 1, 4 )


oneSixth : Fraction
oneSixth =
    ( 1, 6 )


oneEight : Fraction
oneEight =
    ( 1, 8 )


calculateHeritage : Model -> Model
calculateHeritage model =
    model
        |> ifSelectedCalculate Husband husbandHeritage
        |> ifSelectedCalculate Wife wifesHeritage
        |> ifSelectedCalculate Wife motherHeritage
        |> ifSelectedCalculate Wife fatherHeritage
        |> ifSelectedCalculate Son sonsHeritage


ifSelectedCalculate : Heritor -> (Model -> Model) -> Model -> Model
ifSelectedCalculate heritor calculate model =
    case selectedHeritor heritor model.heritors of
        [] ->
            model

        _ :: _ ->
            calculate model


husbandHeritage : Model -> Model
husbandHeritage model =
    let
        share =
            if isAnyChildExist model.heritors then
                oneForth
            else
                half

        calculationResults =
            model.calculationResults ++ [ { heritor = Husband, share = share } ]
    in
        { model | calculationResults = calculationResults }


wifesHeritage : Model -> Model
wifesHeritage model =
    let
        share =
            if isAnyChildExist model.heritors then
                oneEight
            else
                oneForth

        calculationResults =
            model.calculationResults ++ [ { heritor = Wife, share = share } ]
    in
        { model | calculationResults = calculationResults }


motherHeritage : Model -> Model
motherHeritage model =
    let
        -- Todo calculate one of two Umar cases
        share =
            if isAnyChildExist model.heritors || isAnySiblingExist model.heritors then
                oneSixth
            else
                oneThird

        calculationResults =
            model.calculationResults ++ [ { heritor = Mother, share = share } ]
    in
        { model | calculationResults = calculationResults }


fatherHeritage : Model -> Model
fatherHeritage model =
    let
        share =
            if isAnySonExist model.heritors then
                oneSixth
            else
                restHeritage model.calculationResults

        calculationResults =
            model.calculationResults ++ [ { heritor = Father, share = share } ]
    in
        { model | calculationResults = calculationResults }


sonsHeritage : Model -> Model
sonsHeritage model =
    let
        restOfShare =
            restHeritage model.calculationResults

        calculationResults =
            model.calculationResults ++ [ { heritor = Son, share = restOfShare } ]
    in
        { model | calculationResults = calculationResults }


restHeritage : List HeritageCalculationResult -> Fraction
restHeritage calculationResults =
    let
        calculateRest { share } restOfShare =
            Fraction.subtract restOfShare share
    in
        List.foldl calculateRest whole calculationResults


selectedHeritor : Heritor -> List HeritorState -> List HeritorState
selectedHeritor heritor heritors =
    let
        selectedHeritor state =
            state.selected == Selected && state.heritor == heritor
    in
        List.filter selectedHeritor heritors


isHeritorsExist : List HeritorState -> List Heritor -> Bool
isHeritorsExist heritorStates heritors =
    let
        isSelectedHeritorInList state =
            state.selected == Selected && List.member state.heritor heritors

        children =
            List.filter isSelectedHeritorInList heritorStates
    in
        List.length children > 0


isAnySonExist : List HeritorState -> Bool
isAnySonExist heritors =
    List.length (selectedHeritor Son heritors) > 0


isAnyChildExist : List HeritorState -> Bool
isAnyChildExist heritorStates =
    isHeritorsExist heritorStates [ Son, Daughter ]


isAnySiblingExist : List HeritorState -> Bool
isAnySiblingExist heritorStates =
    [ FullBrother, BrotherByFather, BrotherByMother, FullSister, SisterByFather, SisterByMother ]
        |> isHeritorsExist heritorStates
