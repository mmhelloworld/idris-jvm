module Main

import Java.Lang
import System.FFI

%export """
    jvm:import
    io/github/mmhelloworld/test/Holder
    """
jvmImports : List String
jvmImports = []

%export """
        jvm:public Holder
        {
            "annotations": [ {"NoArgsConstructor": {}} ],
            "fields": {
                "value": { "type": "Object", "modifiers": ["public", "static"] }
            }
        }
        """
public export
Holder : Type
Holder = Struct "io/github/mmhelloworld/test/Holder" []

%foreign "jvm:#=value(java/lang/Object void),io/github/mmhelloworld/test/Holder"
prim_setValue : Object -> PrimIO ()

%foreign "jvm:#value(java/lang/Object),io/github/mmhelloworld/test/Holder"
prim_getValue : PrimIO Object

%export """
        jvm:public append
        {
            "enclosingType": "Holder",
            "arguments": [ { "type": "Holder" }, { "type": "String" } ],
            "returnType": "String"
        }
        """
appendExport : Holder -> String -> PrimIO String
appendExport this s = toPrim $ pure (s ++ "!")

%foreign "jvm:.append"
prim_callAppend : Holder -> String -> PrimIO String

%foreign "jvm:<init>"
prim_newHolder : PrimIO Holder

main : IO ()
main = do
    primIO $ prim_setValue (believe_me "stored")
    v <- primIO prim_getValue
    putStrLn $ "field roundtrip: " ++ believe_me v
    holder <- primIO prim_newHolder
    r <- primIO $ prim_callAppend holder "ping"
    putStrLn $ "export roundtrip: " ++ r
