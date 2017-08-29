module Utils.Fraction exposing (Fraction, add, subtract)


type alias Fraction =
    ( Numerator, Denominator )


type alias Numerator =
    Int


type alias Denominator =
    Int


add : Fraction -> Fraction -> Fraction
add firstFraction secondFraction =
    let
        ( ( firstNumerator, firstDenominator ), ( secondNumerator, _ ) ) =
            toCommonDenominator firstFraction secondFraction
    in
        ( firstNumerator + secondNumerator, firstDenominator ) |> normalize


subtract : Fraction -> Fraction -> Fraction
subtract firstFraction secondFraction =
    let
        ( ( firstNumerator, firstDenominator ), ( secondNumerator, _ ) ) =
            toCommonDenominator firstFraction secondFraction
    in
        ( firstNumerator - secondNumerator, firstDenominator ) |> normalize


toCommonDenominator : Fraction -> Fraction -> ( Fraction, Fraction )
toCommonDenominator ( firstNumerator, firstDenominator ) ( secondNumerator, secondDenomenator ) =
    ( ( firstNumerator * secondDenomenator, firstDenominator * secondDenomenator )
    , ( secondNumerator * firstDenominator, secondDenomenator * firstDenominator )
    )


normalize : Fraction -> Fraction
normalize fraction =
    let
        gcd =
            greatestCommonDivisor fraction

        ( numerator, denominator ) =
            fraction
    in
        ( numerator // gcd, denominator // gcd )


greatestCommonDivisor : Fraction -> Int
greatestCommonDivisor ( numerator, denominator ) =
    if denominator == 0 then
        numerator
    else
        greatestCommonDivisor ( denominator, rem numerator denominator )
