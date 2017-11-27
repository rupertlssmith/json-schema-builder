module JsonSchema.Builder
    exposing
        ( build
        , object
        , with
        , field
        , string
        , integer
        , number
        , boolean
        , Struc(..)
        )

{-| Module docs

@docs Struc
@docs string, integer, number, boolean
@docs build, object, with, field

-}


{-| Struc specifications.
-}
type Struc obj
    = IntStruc (obj -> Int)
    | StrStruc (obj -> String)
    | NumStruc (obj -> Float)
    | BoolStruc (obj -> Bool)
    | Object (List ( String, Struc obj ))


{-| Runs the builder.
-}
build : List ( String, Struc obj ) -> Struc obj
build fieldSpecs =
    Object fieldSpecs


{-| Builds an object.
-}
object : cons -> List ( String, Struc obj )
object _ =
    []


{-| Adds fields to an object.
-}
with : List ( String, Struc obj ) -> List ( String, Struc obj ) -> List ( String, Struc obj )
with fieldEncoder remainderEncoder =
    List.append (fieldEncoder) (remainderEncoder)


{-| Builds a field.
-}
field : String -> (obj -> field) -> ((obj -> field) -> Struc obj) -> List ( String, Struc obj )
field name lens encoder =
    encode name lens encoder |> List.singleton


encode : String -> (obj -> field) -> ((obj -> field) -> Struc obj) -> ( String, Struc obj )
encode name lens encoder =
    ( name, encoder lens )


{-| Builds an integer type.
-}
integer : (obj -> Int) -> Struc obj
integer =
    IntStruc


{-| Builds a string type.
-}
string : (obj -> String) -> Struc obj
string =
    StrStruc


{-| Builds a number (float) type.
-}
number : (obj -> Float) -> Struc obj
number =
    NumStruc


{-| Builds a boolean type.
-}
boolean : (obj -> Bool) -> Struc obj
boolean =
    BoolStruc
