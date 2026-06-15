module Java.Lang

public export
data Object : Type where [external]

public export
data Cloneable : Type where [external]

public export
data Iterable : Type where [external]

public export
interface Inherits child parent where
  constructor MkInherits
  export %inline
  subtyping : child -> parent
  subtyping = believe_me

public export
Inherits a a where

public export
Inherits String Object where
