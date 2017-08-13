module Types exposing (..)


type Msg
    = SelectHeritor (Maybe String)
    | RemoveHeritor HeritorState
    | IncrementHeritor HeritorState
    | DecrementHeritor HeritorState


type alias Model =
    { heritors : List HeritorState }


type alias HeritorState =
    { heritor : Heritor
    , count : Int
    , selected : HeritorSelectionState
    , availableCount : Int
    }


type HeritorSelectionState
    = Selected
    | Unselected


type Heritor
    = Father
    | GrandFather
    | GrandGrandFather
    | Mother
    | GrandMother
    | GrandGrandMother
    | Son
    | GrandSon
    | CrandGrandSon
    | Daughter
    | GrandDaughter
    | GrandGrandDaughter
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
    , ( GrandFather, 1 )
    , ( GrandGrandFather, 1 )
    , ( Mother, 1 )
    , ( GrandMother, 1 )
    , ( GrandGrandMother, 1 )
    , ( Son, 100 )
    , ( GrandSon, 1000 )
    , ( CrandGrandSon, 1000 )
    , ( Daughter, 100 )
    , ( GrandDaughter, 1000 )
    , ( GrandGrandDaughter, 1000 )
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

        GrandFather ->
            "أب الأب"

        GrandGrandFather ->
            "أب الأب الأب"

        Mother ->
            "أم"

        GrandMother ->
            "أم الأب"

        GrandGrandMother ->
            "أم الأب الأب"

        Son ->
            "ابن"

        GrandSon ->
            "ابن لابن"

        CrandGrandSon ->
            "ابن لابن لابن"

        Daughter ->
            "بنت"

        GrandDaughter ->
            "بنت لابن"

        GrandGrandDaughter ->
            "بنت لابن لابن"

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

        "أب الأب" ->
            Just GrandFather

        "أب الأب الأب" ->
            Just GrandGrandFather

        "أم" ->
            Just Mother

        "أم الأب" ->
            Just GrandMother

        "أم الأب الأب" ->
            Just GrandGrandMother

        "ابن" ->
            Just Son

        "ابن لابن" ->
            Just GrandSon

        "ابن لابن لابن" ->
            Just CrandGrandSon

        "بنت" ->
            Just Daughter

        "بنت لابن" ->
            Just GrandDaughter

        "بنت لابن لابن" ->
            Just GrandGrandDaughter

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
