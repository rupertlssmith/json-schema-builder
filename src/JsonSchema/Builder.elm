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


type ObjectBuilder result
    = ObjectBuilder (() -> Decoder result)


type FieldBuilder result
    = FieldBuilder String (() -> Decoder result)


{-| Runs the builder.
-}
build : ObjectBuilder result -> Result result
build (ObjectBuilder decodeF) =
    { decoder = decodeF ()
    }


{-| Builds an object.
-}
object : (fields -> a) -> ObjectBuilder (fields -> a)
object ctr =
    ObjectBuilder (always (Decode.succeed ctr))


map2 : (a -> b -> c) -> ObjectBuilder a -> ObjectBuilder b -> ObjectBuilder c
map2 f (ObjectBuilder decoderA) (ObjectBuilder decoderB) =
    let
        joinedDecoder selectionSet =
            Decode.map2 f (decoderA selectionSet) (decoderB selectionSet)
    in
        ObjectBuilder joinedDecoder


{-| Adds fields to an object.
-}
with : FieldBuilder a -> ObjectBuilder (a -> b) -> ObjectBuilder b
with fieldSpec objectSpec =
    map2 (<|) objectSpec (extract fieldSpec)


extract : FieldBuilder a -> ObjectBuilder a
extract (FieldBuilder field decoder) =
    ObjectBuilder (selectionDecoder field decoder)


selectionDecoder : String -> (() -> Decoder result) -> (() -> Decoder result)
selectionDecoder field decoder =
    Decode.field field << decoder


{-| Builds a field.
-}
field : String -> ObjectBuilder result -> FieldBuilder result
field name (ObjectBuilder decoder) =
    FieldBuilder name decoder


{-| Defines the type of a field as a string.
-}
string : ObjectBuilder String
string =
    primitive Decode.string


primitive : Decoder result -> ObjectBuilder result
primitive decoder =
    ObjectBuilder (always decoder)
