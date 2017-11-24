module Encode exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import JsonSchema.Encode exposing (..)
import Json.Encode exposing (Value)
import Result exposing (Result)


-- Basic types.
-- Objects with simple fields.


type alias ObjectSimpleFields =
    { a : String
    , b : Int
    , c : Float
    , d : Bool
    }


objectSimpleFields =
    { a = "test"
    , b = 2
    , c = 5.678
    , d = True
    }


objectSimpleFieldsEncoder =
    build
        (object ObjectSimpleFields
            |> with (field "a" .a string)
            |> with (field "b" .b integer)
            |> with (field "c" .c number)
            |> with (field "d" .d boolean)
        )


testDecodeObjectSimpleFields : Test
testDecodeObjectSimpleFields =
    test "An object with simple fields encodes." <|
        \_ ->
            let
                value =
                    objectSimpleFieldsEncoder objectSimpleFields

                d =
                    Debug.log "test" value
            in
                Expect.pass



-- Objects with optional fields defaulting to Nothing.
-- Objects with nested objects.


type alias ObjectOuter =
    { inner : ObjectSimpleFields
    }


objectOuter =
    "{ \"inner\" : { \"a\" : \"test\", \"b\" : 2, \"c\" : 5.678, \"d\" : true } }"


objectOuterEncoder =
    build
        (object ObjectOuter
            |> with
                (field "inner"
                    .inner
                    (object ObjectSimpleFields
                        |> with (field "a" .a string)
                        |> with (field "b" .b integer)
                        |> with (field "c" .c number)
                        |> with (field "d" .d boolean)
                    )
                )
        )


testDecodeObjectOuter : Test
testDecodeObjectOuter =
    test "An object with an inner object decodes." <|
        \_ ->
            let
                value =
                    objectOuterEncoder objectOuter

                d =
                    Debug.log "test" value
            in
                Expect.pass
