module JsonSchema.Decode
    exposing
        ( build
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
@docs build, object, with, field

-}

import Json.Decode as Decode exposing (Decoder)


{-| Runs the builder.
-}
build : Decoder a -> Decoder a
build =
    identity


{-| Builds an object.
-}
object : (fields -> a) -> Decoder (fields -> a)
object ctr =
    Decode.succeed ctr


{-| Adds fields to an object.
-}
with : ( String, Decoder a ) -> Decoder (a -> b) -> Decoder b
with ( field, decoder ) objectSpec =
    Decode.map2 (<|) objectSpec (Decode.field field decoder)


{-| Builds a field.
-}
field :
    String
    -> (obj -> a)
    -> Decoder a
    -> ( String, Decoder a )
field name _ decoder =
    ( name, decoder )


{-| Builds a string type.
-}
string : Decoder String
string =
    Decode.string


{-| Builds an integer type.
-}
integer : Decoder Int
integer =
    Decode.int


{-| Builds a number (float) type.
-}
number : Decoder Float
number =
    Decode.float


{-| Builds a boolean type.
-}
boolean : Decoder Bool
boolean =
    Decode.bool
