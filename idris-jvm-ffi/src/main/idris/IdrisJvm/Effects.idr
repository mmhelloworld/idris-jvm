module IdrisJvm.Effects

import Effects
import Effect.System
import Effect.StdIO
import IdrisJvm.IO
import IdrisJvm.System

%access export

Handler StdIO JVM_IO where
  handle () (PutStr s) k = do putStr' s; k () ()
  handle () GetStr     k = do x <- getLine'; k x ()
  handle () (PutCh c)  k = do putCh c; k () ()
  handle () GetCh      k = do x <- getCh; k x ()

implementation Handler System JVM_IO where
    handle () Args k = do x <- getArgs; k x ()
    handle () Time k = do x <- time; k x ()
    handle () (GetEnv s) k = do x <- getEnv s; k x ()
    handle () (CSystem s) k = do x <- system s; k x ()
