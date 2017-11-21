module JsonSchema.Builder
    exposing
        ( Result
        , build
        , object
        , with
        , field
        , string
        )

{-| Module docs

@docs Result, build, object, with, field, string

-}

import JsonSchema exposing (Schema)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


{-| The result.
-}
type alias Result result =
    { --schema : List JsonSchema.ObjectSchemaProperty -> Schema
      --, enocoder : a -> Encode.Value
      decoder : Decoder result
    }


type ValueBuilder result
    = ValueBuilder (() -> Decoder result)


type FieldBuilder result
    = FieldBuilder String (() -> Decoder result)


{-| Runs the builder.
-}
build : ValueBuilder result -> Result result
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


fieldDecoder : String -> (() -> Decoder result) -> (() -> Decoder result)
fieldDecoder field decoder =
    Decode.field field << decoder


{-| Builds a field.
-}
field : String -> ValueBuilder result -> FieldBuilder result
field name (ValueBuilder decoder) =
    FieldBuilder name decoder


{-| Defines the type of a field as a string.
-}
string : ValueBuilder String
string =
    primitive Decode.string


primitive : Decoder result -> ValueBuilder result
primitive decoder =
    ValueBuilder (always decoder)
