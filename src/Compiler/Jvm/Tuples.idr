module Compiler.Jvm.Tuples

public export
second : (a, b, c) -> b
second (_, value, _) = value

public export
updateFirst : c -> (a, b) -> (c, b)
updateFirst v (_, b) = (v, b)

public export
updateSecond : c -> (a, b) -> (a, c)
updateSecond v (a, _) = (a, v)
