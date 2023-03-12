module Java.Lang

public export
interface Inherits child parent where
    constructor MkInherits

    export %inline
    subtyping : child -> parent
    subtyping = believe_me

public export
Inherits a a where

namespace Object
    export
    data Object : Type where [external]

    %foreign "jvm:.toString(java/lang/Object java/lang/String),java/lang/Object"
    prim_toString : Object -> PrimIO String

    export %inline
    toString : (HasIO io, Inherits a Object) => a -> io String
    toString obj = primIO $ prim_toString (subtyping obj)

public export
Inherits a Object where
