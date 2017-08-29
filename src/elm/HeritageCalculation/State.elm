module HeritageCalculation.State exposing (..)

import Types exposing (..)
import Utils.Fraction as Fraction exposing (Fraction)


whole : Fraction
whole =
    ( 1, 1 )


half : Fraction
half =
    ( 1, 2 )


onForth : Fraction
onForth =
    ( 1, 4 )


calculateHeritage : Model -> Model
calculateHeritage model =
    ( model, whole )
        |> husbandHeritage
        |> sonsHeritage
        |> Tuple.first


husbandHeritage : ( Model, ShareOfHeritage ) -> ( Model, ShareOfHeritage )
husbandHeritage ( model, availableShare ) =
    case selectedHeritor Husband model.heritors of
        [] ->
            ( model, availableShare )

        _ :: _ ->
            let
                share =
                    if isChildrenExist model.heritors then
                        onForth
                    else
                        half

                calculationResults =
                    model.calculationResults ++ [ { heritor = Son, share = share } ]

                restOfShare =
                    Fraction.subtract availableShare share
            in
                ( { model | calculationResults = calculationResults }, restOfShare )


sonsHeritage : ( Model, ShareOfHeritage ) -> ( Model, ShareOfHeritage )
sonsHeritage ( model, availableShare ) =
    case selectedHeritor Son model.heritors of
        [] ->
            ( model, availableShare )

        _ :: _ ->
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
