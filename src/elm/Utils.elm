module Utils exposing (..)


valueWithDefault : Maybe a -> a -> a
valueWithDefault maybeSomething default =
    case maybeSomething of
        Just value ->
            value

        Nothing ->
            default
