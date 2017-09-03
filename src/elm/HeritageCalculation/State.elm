module HeritageCalculation.State exposing (..)

import Types exposing (..)
import Utils.Fraction as Fraction exposing (Fraction)


whole : Fraction
whole =
    ( 1, 1 )


half : Fraction
half =
    ( 1, 2 )


oneForth : Fraction
oneForth =
    ( 1, 4 )


oneEight : Fraction
oneEight =
    ( 1, 8 )


calculateHeritage : Model -> Model
calculateHeritage model =
    ( model, whole )
        |> ifSelectedCalculate Husband husbandHeritage
        |> ifSelectedCalculate Wife wifesHeritage
        |> ifSelectedCalculate Son sonsHeritage
        |> Tuple.first


ifSelectedCalculate :
    Heritor
    -> (( Model, ShareOfHeritage ) -> ( Model, ShareOfHeritage ))
    -> ( Model, ShareOfHeritage )
    -> ( Model, ShareOfHeritage )
ifSelectedCalculate heritor calculate ( model, availableShare ) =
    case selectedHeritor heritor model.heritors of
        [] ->
            ( model, availableShare )

        _ :: _ ->
            calculate ( model, availableShare )


husbandHeritage : ( Model, ShareOfHeritage ) -> ( Model, ShareOfHeritage )
husbandHeritage ( model, availableShare ) =
    let
        share =
            if isChildrenExist model.heritors then
                oneForth
            else
                half

        calculationResults =
            model.calculationResults ++ [ { heritor = Husband, share = share } ]

        restOfShare =
            Fraction.subtract availableShare share
    in
        ( { model | calculationResults = calculationResults }, restOfShare )


wifesHeritage : ( Model, ShareOfHeritage ) -> ( Model, ShareOfHeritage )
wifesHeritage ( model, availableShare ) =
    let
        share =
            if isChildrenExist model.heritors then
                oneEight
            else
                oneForth

        calculationResults =
            model.calculationResults ++ [ { heritor = Wife, share = share } ]

        restOfShare =
            Fraction.subtract availableShare share
    in
        ( { model | calculationResults = calculationResults }, restOfShare )


sonsHeritage : ( Model, ShareOfHeritage ) -> ( Model, ShareOfHeritage )
sonsHeritage ( model, availableShare ) =
    let
        calculationResults =
            model.calculationResults ++ [ { heritor = Son, share = availableShare } ]
    in
        ( { model | calculationResults = calculationResults }, ( 0, 0 ) )


selectedHeritor : Heritor -> List HeritorState -> List HeritorState
selectedHeritor heritor heritors =
    let
        selectedHeritor state =
            state.selected == Selected && state.heritor == heritor
    in
        List.filter selectedHeritor heritors


isChildrenExist : List HeritorState -> Bool
isChildrenExist heritors =
    let
        sonOrDaughter state =
            state.selected == Selected && (state.heritor == Son || state.heritor == Daughter)

        children =
            List.filter sonOrDaughter heritors
    in
        (List.length children) > 0
