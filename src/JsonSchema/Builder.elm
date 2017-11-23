module JsonSchema.Builder
    exposing
        ( Result
        , build
        , object
        , with
        , field
        , string
        , integer
        , number
        , boolean
        )

{-| Module docs

@docs string, integer, number, boolean
@docs Result, build, object, with, field

-}

import JsonSchema exposing (Schema)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias ObjectSimpleFields =
    { a : String
    , b : Int
    , c : Float
    , d : Bool
    }


obj : ValueBuilder (String -> Int -> Float -> Bool -> ObjectSimpleFields)
obj =
    (object ObjectSimpleFields)


one : ValueBuilder (Int -> Float -> Bool -> ObjectSimpleFields)
one =
    (object ObjectSimpleFields)
        |> with (field "a" .a string)


two : ValueBuilder (Float -> Bool -> ObjectSimpleFields)
two =
    ((object ObjectSimpleFields)
        |> with (field "a" .a string)
    )
        |> with (field "b" .b integer)


strf : ValueBuilder (String -> b) -> ValueBuilder b
strf =
    with (field "a" .a string)


intf : ValueBuilder (Int -> b) -> ValueBuilder b
intf =
    with (field "b" .b integer)


numf : ValueBuilder (Float -> b) -> ValueBuilder b
numf =
    with (field "c" .c number)


{-| The result.
-}
type alias Result a =
    { --schema : List JsonSchema.ObjectSchemaProperty -> Schema
      --, enocoder : a -> Encode.Value
      decoder : Decoder a
    }


type ValueBuilder a
    = ValueBuilder (() -> Decoder a)


type FieldBuilder a
    = FieldBuilder String (() -> Decoder a)


{-| Runs the builder.
-}
build : ValueBuilder a -> Result a
build (ValueBuilder decodeF) =
    { decoder = decodeF ()
    }


{-| Builds an object.
-}
object : (fields -> a) -> ValueBuilder (fields -> a)
object ctr =
    ValueBuilder (always (Decode.succeed ctr))


map2 : (a -> b -> c) -> ValueBuilder a -> ValueBuilder b -> ValueBuilder c
map2 f (ValueBuilder decoderA) (ValueBuilder decoderB) =
    let
        joinedDecoder _ =
            Decode.map2 f (decoderA ()) (decoderB ())
    in
        ValueBuilder joinedDecoder


{-| Adds fields to an object.
-}
with : FieldBuilder a -> ValueBuilder (a -> b) -> ValueBuilder b
with fieldSpec objectSpec =
    map2 (<|) objectSpec (extract fieldSpec)


extract : FieldBuilder a -> ValueBuilder a
extract (FieldBuilder field decoder) =
    ValueBuilder (fieldDecoder field decoder)


fieldDecoder : String -> (() -> Decoder a) -> (() -> Decoder a)
fieldDecoder field decoder =
    Decode.field field << decoder


{-| Builds a field.
-}
field : String -> ext -> ValueBuilder a -> FieldBuilder a
field name _ (ValueBuilder decoder) =
    FieldBuilder name decoder


{-| Builds a string type.
-}
string : ValueBuilder String
string =
    primitive Decode.string


{-| Builds an integer type.
-}
integer : ValueBuilder Int
integer =
    primitive Decode.int


{-| Builds a number (float) type.
-}
number : ValueBuilder Float
number =
    primitive Decode.float


{-| Builds a boolean type.
-}
boolean : ValueBuilder Bool
boolean =
    primitive Decode.bool


primitive : Decoder a -> ValueBuilder a
primitive decoder =
    ValueBuilder (always decoder)
