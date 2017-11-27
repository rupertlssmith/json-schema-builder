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


{-| Object specifications.
-}
type Object
    = Nah



--
--
--type Field obj
--    = IntField String (obj -> Int)
--    | StrField String (obj -> String)
--    | NumField String (obj -> Float)
--    | BoolField String (obj -> Bool)


{-| Field specifications.
-}
type Field obj
    = IntField (obj -> Int)
    | StrField (obj -> String)
    | NumField (obj -> Float)
    | BoolField (obj -> Bool)
    | Object (List ( String, Field obj ))


{-| Runs the builder.
-}
build : List ( String, Field obj ) -> Field obj
build fieldEncoder =
    fieldEncoder |> encodeObject


encodeObject : List ( String, Field obj ) -> Field obj
encodeObject fields =
    Object fields


{-| Builds an object.
-}
object : cons -> List ( String, Field obj )
object _ =
    []


{-| Adds fields to an object.
-}
with : List ( String, Field obj ) -> List ( String, Field obj ) -> List ( String, Field obj )
with fieldEncoder remainderEncoder =
    List.append (fieldEncoder) (remainderEncoder)


{-| Builds a field.
-}
field :
    String
    -> (obj -> field)
    -> ((obj -> field) -> Field obj)
    -> List ( String, Field obj )
field name lens encoder =
    encode name lens encoder |> List.singleton


encode : String -> (obj -> field) -> ((obj -> field) -> Field obj) -> ( String, Field obj )
encode name lens encoder =
    ( name, encoder lens )


{-| Builds an integer type.
-}
integer : (obj -> Int) -> Field obj
integer lens =
    encodeInt lens


{-| Builds a string type.
-}
string : (obj -> String) -> Field obj
string lens =
    encodeString lens


{-| Builds a number (float) type.
-}
number : (obj -> Float) -> Field obj
number lens =
    encodeFloat lens


{-| Builds a boolean type.
-}
boolean : (obj -> Bool) -> Field obj
boolean lens =
    encodeBool lens


encodeInt lens =
    IntField lens


encodeString lens =
    StrField lens


encodeFloat lens =
    NumField lens


encodeBool lens =
    BoolField lens
