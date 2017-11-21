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



-- type SchemaType
--     = ObjectBuilder (List JsonSchema.ObjectSchemaProperty -> Schema)


object : (fields -> a) -> Builder (fields -> a)
object ctr =
    Builder (always (Decode.succeed ctr))
