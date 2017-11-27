module Builder exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import JsonSchema.Builder exposing (..)
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


objectSimpleFieldsSpec =
    object ObjectSimpleFields
        |> with (field "a" .a string)
        |> with (field "b" .b integer)
        |> with (field "c" .c number)
        |> with (field "d" .d boolean)
        |> build


testDecodeObjectSimpleFields : Test
testDecodeObjectSimpleFields =
    test "An object with simple fields builds." <|
        \_ ->
            let
                value =
                    objectSimpleFieldsSpec

                d =
                    Debug.log "test" value
            in
                Expect.pass



-- Objects with optional fields defaulting to Nothing.
-- Objects with nested objects.


type alias ObjectOuter =
    { inner : ObjectSimpleFields
    }


objectOuterFieldSpec =
    object ObjectOuter
        |> with
            (field "inner"
                .inner
                (object ObjectSimpleFields
                    |> with (field "a" .a string)
                    |> with (field "b" .b integer)
                    |> with (field "c" .c number)
                    |> with (field "d" .d boolean)
                    |> build
                )
            )
        |> build


testDecodeObjectOuter : Test
testDecodeObjectOuter =
    test "An object with an inner object decodes." <|
        \_ ->
            let
                value =
                    objectOuterFieldSpec

                d =
                    Debug.log "test" value
            in
                Expect.pass



-- Objects with mutually recursive relationships.
-- Arrays of simple types.
-- String formats.
-- Required and optional fields.
-- Value restrictions.
-- Object property restrictions.
-- Null, oneOf, allOf and anyOf.
