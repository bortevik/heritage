module Types exposing (..)


type Msg
    = SelectHeritor (Maybe String)
    | RemoveHeritor HeritorState
    | IncrementHeritor HeritorState
    | DecrementHeritor HeritorState
    | CalculateHeritage


type alias Model =
    { heritors : List HeritorState
    , calculationResults : List HeritageCalculationResult
    }


type alias HeritorState =
    { heritor : Heritor
    , count : Int
    , selected : HeritorSelectionState
    , availableCount : Int
    }


type HeritorSelectionState
    = Selected
    | Unselected


type alias HeritageCalculationResult =
    { heritor : Heritor
    , share : ShareOfHeritage
    }


type alias ShareOfHeritage =
    ( Int, Int )


type Heritor
    = Father
    | Mother
    | Son
    | Daughter
    | Wife
    | Husband
    | FullBrother
    | BrotherByFather
    | BrotherByMother
    | FullSister
    | SisterByFather
    | SisterByMother
    | UncleByFather
    | UncleByMother
    | AuntByFather
    | AuntByMother


heritorsAvailabeCounts : List ( Heritor, Int )
heritorsAvailabeCounts =
    [ ( Father, 1 )
    , ( Mother, 1 )
    , ( Son, 100 )
    , ( Daughter, 100 )
    , ( Wife, 4 )
    , ( Husband, 1 )
    , ( FullBrother, 20 )
    , ( BrotherByFather, 20 )
    , ( BrotherByMother, 20 )
    , ( FullSister, 20 )
    , ( SisterByFather, 20 )
    , ( SisterByMother, 20 )
    , ( UncleByFather, 20 )
    , ( UncleByMother, 20 )
    , ( AuntByFather, 20 )
    , ( AuntByMother, 20 )
    ]


heritorToString : Heritor -> String
heritorToString heritor =
    case heritor of
        Father ->
            "أب"

        Mother ->
            "أم"

        Son ->
            "ابن"

        Daughter ->
            "بنت"

        Wife ->
            "زوجة"

        Husband ->
            "زوج"

        FullBrother ->
            "أخ شقيق"

        BrotherByFather ->
            "أخ لأب"

        BrotherByMother ->
            "أخ لأم"

        FullSister ->
            "أخت"

        SisterByFather ->
            "أخت لأب"

        SisterByMother ->
            "أخت لأم"

        UncleByFather ->
            "عم"

        UncleByMother ->
            "خال"

        AuntByFather ->
            "عمة"

        AuntByMother ->
            "خالة"


heritorFromString : String -> Maybe Heritor
heritorFromString heritor =
    case heritor of
        "أب" ->
            Just Father

        "أم" ->
            Just Mother

        "ابن" ->
            Just Son

        "بنت" ->
            Just Daughter

        "زوجة" ->
            Just Wife

        "زوج" ->
            Just Husband

        "أخ شقيق" ->
            Just FullBrother

        "أخ لأب" ->
            Just BrotherByFather

        "أخ لأم" ->
            Just BrotherByMother

        "أخت" ->
            Just FullSister

        "أخت لأب" ->
            Just SisterByFather

        "أخت لأم" ->
            Just SisterByMother

        "عم" ->
            Just UncleByFather

        "خال" ->
            Just UncleByMother

        "عمة" ->
            Just AuntByFather

        "خالة" ->
            Just AuntByMother

        _ ->
            Nothing
