module Main

-- Com.Example is generated from greeter.jar via `--jvm-classpath ... --jvm-ffi-import`.
import Com.Example

main : IO ()
main = do
  g <- Greeter.of_ "World"          -- static factory: of(String) -> Greeter
  msg <- Greeter.greet g "Hello"    -- instance: greet(String) -> String
  putStrLn msg
  n <- Greeter.nameLength g
  printLn n
