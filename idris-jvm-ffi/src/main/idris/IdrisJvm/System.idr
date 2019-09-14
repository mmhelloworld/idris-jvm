module IdrisJvm.System

import public Data.So
import IdrisJvm.IO
import Java.Util
import Java.Math
import Java.Lang

%access public export

getArgs : JVM_IO (List String)
getArgs = do
    argsList <- invokeStatic RuntimeClass "getProgramArgs" (JVM_IO JList)
    Iterator.toList !(JList.iterator argsList)

time : JVM_IO Integer
time = believe_me <$> invokeStatic RuntimeClass "time" (JVM_IO BigInteger)

getEnv : String -> JVM_IO (Maybe String)
getEnv = System.getenv

system : String -> JVM_IO Int
system = invokeStatic RuntimeClass "runCommand" (String -> JVM_IO Int)

usleep : (i : Int) -> { auto prf : So (i >= 0 && i <= 1000000) } -> JVM_IO ()
usleep interval = invokeStatic RuntimeClass "usleep" (Int -> JVM_IO ()) interval

exit : Int -> JVM_IO a
exit = believe_me . Java.Lang.System.exit

||| Programs can either terminate successfully, or end in a caught
||| failure.
data ExitCode : Type where
  ||| Terminate successfully.
  ExitSuccess : ExitCode
  ||| Program terminated for some prescribed reason.
  |||
  ||| @errNo A non-zero numerical value indicating failure.
  ||| @prf   Proof that the int value is non-zero.
  ExitFailure : (errNo    : Int)
             -> {auto prf : So (not $ errNo == 0)}
             -> ExitCode

||| Terminate the program with an `ExitCode`. This code indicates the
||| success of the program's execution, and returns the success code
||| to the program's caller.
|||
||| @code The `ExitCode` for program.
exitWith : (code : ExitCode) -> JVM_IO a
exitWith ExitSuccess         = exit 0
exitWith (ExitFailure errNo) = exit errNo

||| Exit the program indicating failure.
exitFailure : JVM_IO a
exitFailure = exitWith (ExitFailure 1)

||| Exit the program after a successful run.
exitSuccess : JVM_IO a
exitSuccess = exitWith ExitSuccess

||| Wall clock time
record Clock where
  constructor MkClock
  seconds : Integer
  nanoseconds : Integer

||| Get the system's wall clock time.
clockTime : JVM_IO Clock
clockTime =
  believe_me <$> invokeStatic (Class "io/github/mmhelloworld/idrisjvm/runtime/IdrisSystem")
    "getIdrisClock" (JVM_IO IdrisObject)
