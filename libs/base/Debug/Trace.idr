module Debug.Trace

import Prelude
import PrimIO

%default total

export
trace : (msg : String) -> (result : a) -> a
trace x val = unsafePerformIO (do putStrLn x; pure val)

export %inline
traceValBy : (msgF : a -> String) -> (result : a) -> a
traceValBy f v = trace (f v) v

export %inline
traceVal : Show a => a -> a
traceVal = traceValBy show

%foreign "jvm:toString(java/lang/Object java/lang/String),java/util/Objects"
prim_objectToString : AnyPtr -> PrimIO String

export
toString : a -> String
toString value = unsafePerformIO $ primIO (prim_objectToString (believe_me value))
