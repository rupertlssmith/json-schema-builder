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
        , Object(..)
        , Field(..)
        )

{-| Module docs

@docs Object, Field
@docs string, integer, number, boolean
@docs build, object, with, field

-}


{-| Object specifications.
-}
type Object obj fields
    = Object (fields -> obj) (List Field)


{-| Field specifications.
-}
type Field obj
    = IntField String (obj -> Int)
    | StrField String (obj -> String)
    | NumField String (obj -> Float)
    | BoolField String (obj -> Bool)


{-| Runs the builder.
-}
build : Object obj fields -> Object obj fields
build objectSpec =
    objectSpec


{-| Builds an object.
-}
object : (fields -> obj) -> Object obj fields
object cons =
    Object cons []


{-| Adds fields to an object.
-}
with : (b -> List a) -> (b -> List a) -> b -> List a
with fieldSpec remainderSpec obj =
    List.append (fieldSpec obj) (remainderSpec obj)


{-| Builds a field.
-}
field : a -> b -> (a -> b -> c) -> List c
field name lens fieldType =
    List.singleton <|
        (fieldType name) lens


{-| Builds an integer type.
-}
integer : String -> (obj -> Int) -> Field obj
integer name =
    IntField name


{-| Builds a string type.
-}
string : String -> (obj -> String) -> Field obj
string name =
    StrField name


{-| Builds a number (float) type.
-}
number : String -> (obj -> Float) -> Field obj
number name =
    NumField name


{-| Builds a boolean type.
-}
boolean : String -> (obj -> Bool) -> Field obj
boolean name =
    BoolField name
