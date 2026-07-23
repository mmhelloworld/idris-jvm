module Main

%export """
        jvm:public static isPositive
        {
            "enclosingType": "io/github/mmhelloworld/test/BoolExports",
            "arguments": [ { "type": "int" } ],
            "returnType": "java/lang/Boolean"
        }
        """
isPositive : Int -> Bool
isPositive x = x > 0

%export """
        jvm:public static bothTrue
        {
            "enclosingType": "io/github/mmhelloworld/test/BoolExports",
            "arguments": [ { "type": "java/lang/Boolean" }, { "type": "java/lang/Boolean" } ],
            "returnType": "boolean"
        }
        """
bothTrue : Bool -> Bool -> Bool
bothTrue x y = x && y

%foreign "jvm:isPositive(int java/lang/Boolean),io/github/mmhelloworld/test/BoolExports"
prim_isPositive : Int -> PrimIO Bool

%foreign "jvm:bothTrue(java/lang/Boolean java/lang/Boolean boolean),io/github/mmhelloworld/test/BoolExports"
prim_bothTrue : Bool -> Bool -> PrimIO Bool

main : IO ()
main = do
    positive <- primIO $ prim_isPositive 5
    negative <- primIO $ prim_isPositive (-5)
    printLn (positive, negative)
    both <- primIO $ prim_bothTrue True True
    oneFalse <- primIO $ prim_bothTrue True False
    printLn (both, oneFalse)
