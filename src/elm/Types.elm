module Types exposing (..)


type Msg
    = AddHeritor (Maybe String)
    | RemoveHeritor HeritorState
    | IncrementHeritor HeritorState
    | DecrementHeritor HeritorState


type alias Model =
    { heritors : List HeritorState }


type alias HeritorState =
    { heritor : Heritor
    , count : Int
    }


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


heritors : List HeritorState
heritors =
    [ { heritor = Father, count = 1 }
    , { heritor = GrandFather, count = 1 }
    , { heritor = GrandGrandFather, count = 1 }
    , { heritor = Mother, count = 1 }
    , { heritor = GrandMother, count = 1 }
    , { heritor = GrandGrandMother, count = 1 }
    , { heritor = Son, count = 100 }
    , { heritor = GrandSon, count = 1000 }
    , { heritor = CrandGrandSon, count = 10000 }
    , { heritor = Daughter, count = 100 }
    , { heritor = GrandDaughter, count = 1000 }
    , { heritor = GrandGrandDaughter, count = 10000 }
    , { heritor = Wife, count = 4 }
    , { heritor = Husband, count = 1 }
    , { heritor = FullBrother, count = 20 }
    , { heritor = BrotherByFather, count = 20 }
    , { heritor = BrotherByMother, count = 20 }
    , { heritor = FullSister, count = 20 }
    , { heritor = SisterByFather, count = 20 }
    , { heritor = SisterByMother, count = 20 }
    , { heritor = UncleByFather, count = 20 }
    , { heritor = UncleByMother, count = 20 }
    , { heritor = AuntByFather, count = 20 }
    , { heritor = AuntByMother, count = 20 }
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
