module IdrisJvm.System

import public Data.So
import IdrisJvm.IO
import Java.Util
import Java.Math
import Java.Lang

%access public export

getArgs : JVM_IO (List String)
getArgs = do
    argsList <- getStaticField RuntimeClass "programArgs" (JVM_IO JList)
    Iterator.toList !(JList.iterator argsList)

time : JVM_IO Integer
time = believe_me <$> invokeStatic RuntimeClass "time" (JVM_IO BigInteger)

getEnv : String -> JVM_IO (Maybe String)
getEnv = System.getenv

system : String -> JVM_IO Int
system = invokeStatic RuntimeClass "runCommand" (String -> JVM_IO Int)

usleep : (i : Int) -> { auto prf : So (i >= 0 && i <= 1000000) } -> JVM_IO ()
usleep interval = invokeStatic RuntimeClass "usleep" (Int -> JVM_IO ()) interval
