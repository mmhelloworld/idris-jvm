module Main

import Data.IORef

-- Two structurally identical effectful constants must remain distinct:
-- CSE lifting them into one shared toplevel constant would alias the refs.
%noinline
ref1 : IORef (Maybe Int)
ref1 = unsafePerformIO $ newIORef Nothing

%noinline
ref2 : IORef (Maybe Int)
ref2 = unsafePerformIO $ newIORef Nothing

main : IO ()
main = do
    writeIORef ref1 (Just 42)
    v <- readIORef ref2
    putStrLn $ "ref2 after writing ref1: " ++ show v
