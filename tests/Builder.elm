module Builder exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import JsonSchema.Builder exposing (..)
import Json.Decode as Decode
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
    "{ \"a\" : \"test\", \"b\" : 2, \"c\" : 5.678, \"d\" : true }"


objectSimpleFieldsResult =
    build
        (object ObjectSimpleFields
            |> with (field "a" string)
            |> with (field "b" integer)
            |> with (field "c" number)
            |> with (field "d" boolean)
        )


testDecodeObjectSimpleFields : Test
testDecodeObjectSimpleFields =
    test "An object with simple fields decodes." <|
        \_ ->
            case Decode.decodeString objectSimpleFieldsResult.decoder objectSimpleFields of
                Ok object ->
                    Expect.all
                        [ .a >> Expect.equal "test"
                        , .b >> Expect.equal 2
                        , .c >> Expect.equal 5.678
                        , .d >> Expect.equal True
                        ]
                        object

                Err error ->
                    Expect.fail <| "Failed to decode:" ++ error



-- Objects with optional fields defaulting to Nothing.
-- Objects with nested objects.


type alias ObjectOuter =
    { inner : ObjectSimpleFields
    }


objectOuter =
    "{ \"inner\" : { \"a\" : \"test\", \"b\" : 2, \"c\" : 5.678, \"d\" : true } }"


objectOuterResult =
    build
        (object ObjectOuter
            |> with
                (field "inner"
                    (object ObjectSimpleFields
                        |> with (field "a" string)
                        |> with (field "b" integer)
                        |> with (field "c" number)
                        |> with (field "d" boolean)
                    )
                )
        )


testDecodeObjectOuter : Test
testDecodeObjectOuter =
    test "An object with an inner object decodes." <|
        \_ ->
            case Decode.decodeString objectOuterResult.decoder objectOuter of
                Ok object ->
                    Expect.all
                        [ .inner >> .a >> Expect.equal "test"
                        , .inner >> .b >> Expect.equal 2
                        , .inner >> .c >> Expect.equal 5.678
                        , .inner >> .d >> Expect.equal True
                        ]
                        object

                Err error ->
                    Expect.fail <| "Failed to decode:" ++ error



-- Objects with mutually recursive relationships.
-- Arrays of simple types.
-- String formats.
-- Required and optional fields.
-- Value restrictions.
-- Object property restrictions.
-- Null, oneOf, allOf and anyOf.
