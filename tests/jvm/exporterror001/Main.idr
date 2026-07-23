module Main

%export """
    jvm:public isPositive
    {
        "arguments": [ { "type": "int" } ],
        "returnType": "boolean"
    }
    """
isPositive : Int -> Bool
isPositive x = x > 0

main : IO ()
main = printLn (isPositive 5)
