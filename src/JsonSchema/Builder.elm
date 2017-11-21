module JsonSchema.Builder exposing (Result)

{-| Module docs

@docs Result

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


type Builder result
    = Builder (() -> Decoder result)


object : (fields -> a) -> Builder (fields -> a)
object ctr =
    Builder (always (Decode.succeed ctr))


map2 :
    (a -> b -> c)
    -> Builder a
    -> Builder b
    -> Builder c
map2 f (Builder decoderA) (Builder decoderB) =
    let
        joinedDecoder selectionSet =
            Decode.map2 f (decoderA selectionSet) (decoderB selectionSet)
    in
        Builder joinedDecoder


with :
    Builder a
    -> Builder (a -> b)
    -> Builder b
with selection objectSpec =
    map2 (<|) objectSpec selection


field :
    String
    -> Builder result
    -> Builder result
field name (Builder decoder) =
    Builder
        decoder


string : Builder String
string =
    primitive Decode.string


primitive : Decoder result -> Builder result
primitive decoder =
    Builder
        (always decoder)



---


type alias Test =
    { a : String }


test =
    object Test
        |> with (field "a" string)
