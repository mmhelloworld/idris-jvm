module IdrisJvm.Effects

import Effects
import Effect.StdIO
import IdrisJvm.IO

%access export

Handler StdIO JVM_IO where
  handle () (PutStr s) k = do putStr' s; k () ()
  handle () GetStr     k = do x <- getLine'; k x ()
  handle () (PutCh c)  k = do putCh c; k () ()
  handle () GetCh      k = do x <- getCh; k x ()
