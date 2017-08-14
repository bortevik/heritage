module HeritageCalculation.State exposing (..)

import Types exposing (..)


calculateHeritage : Model -> Model
calculateHeritage model =
    ( model, ( 1, 1 ) )
        |> calculateSonsHeritage
        |> Tuple.first


calculateSonsHeritage : ( Model, ShareOfHeritage ) -> ( Model, ShareOfHeritage )
calculateSonsHeritage ( model, restOfShare ) =
    let
        selectedSon { selected, heritor } =
            selected == Selected && heritor == Son

        sons =
            List.filter selectedSon model.heritors
    in
        if List.length sons > 0 then
            let
                calculationResults =
                    model.calculationResults ++ [ { heritor = Son, share = restOfShare } ]
            in
                ( { model | calculationResults = calculationResults }, ( 0, 0 ) )
        else
            ( model, restOfShare )
