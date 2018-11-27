module IdrisJvm.System

import public Data.So
import IdrisJvm.IO
import Java.Util
import Java.Math
import Java.Lang

public export
getArgs : JVM_IO (List String)
getArgs = do
    argsList <- invokeStatic RuntimeClass "getProgramArgs" (JVM_IO JList)
    Iterator.toList !(JList.iterator argsList)

public export
time : JVM_IO Integer
time = believe_me <$> invokeStatic RuntimeClass "time" (JVM_IO BigInteger)

public export
getEnv : String -> JVM_IO (Maybe String)
getEnv = System.getenv

public export
system : String -> JVM_IO Int
system = invokeStatic RuntimeClass "runCommand" (String -> JVM_IO Int)

public export
usleep : (i : Int) -> { auto prf : So (i >= 0 && i <= 1000000) } -> JVM_IO ()
usleep interval = invokeStatic RuntimeClass "usleep" (Int -> JVM_IO ()) interval

public export
exit : Int -> JVM_IO a
exit = believe_me . Java.Lang.System.exit
