{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module IdrisJvm.Codegen.Types where

import           Control.Monad.RWS
import qualified Data.DList                 as DL
import qualified Data.Map                   as Map
import           Idris.Core.TT
import           IdrisJvm.Codegen.Assembler

data CgEnv = CgEnv { className :: String } deriving Show

data CgState = CgState { cgStLambdaIndex     :: Map.Map ClassName Int
                       , cgStSwitchIndex     :: Int
                       , cgStIfIndex         :: Int
                       , cgStFunctionName    :: JMethodName
                       , cgStLocalVarCount   :: Int
                       , shouldDescribeFrame :: Bool
                       , cgStFunctionArgs    :: [Name]
                       } deriving Show

data JMethodName =
  JMethodName { jmethClsName :: String
              , jmethName    :: String
              } deriving (Eq, Show)

initialCgState :: CgState
initialCgState = CgState Map.empty 0 0 (JMethodName "" "") 0 True []

rtClassSig :: String -> String
rtClassSig c = "mmhelloworld/idrisjvmruntime/" ++ c

rtFuncSig :: String
rtFuncSig = "L" ++ rtClassSig "Function" ++ ";"

rtThunkSig :: String
rtThunkSig = "L" ++ rtClassSig "Thunk" ++ ";"

createThunkSig :: String
createThunkSig = "(" ++ rtFuncSig ++ "[Ljava/lang/Object;)" ++ rtThunkSig

freshLambdaIndex :: ClassName -> Cg Int
freshLambdaIndex clazz = do
  classLambdaMap <- cgStLambdaIndex <$> get
  let li = Map.findWithDefault 0 clazz classLambdaMap
  modify . updateLambdaIndex $ Map.alter (Just . maybe 1 succ) clazz
  return li

freshSwitchIndex :: Cg Int
freshSwitchIndex = do
  si <- cgStSwitchIndex <$> get
  modify (updateSwitchIndex succ)
  return si

freshIfIndex :: Cg Int
freshIfIndex = do
  ifIndex <- cgStIfIndex <$> get
  modify (updateIfIndex succ)
  return ifIndex

updateFunctionName :: (JMethodName -> JMethodName) -> CgState -> CgState
updateFunctionName f cgState = cgState { cgStFunctionName = f (cgStFunctionName cgState)}

updateLocalVarCount :: (Int -> Int) -> CgState -> CgState
updateLocalVarCount f cgState = cgState { cgStLocalVarCount = f (cgStLocalVarCount cgState)}

updateSwitchIndex :: (Int -> Int) -> CgState -> CgState
updateSwitchIndex f cgState = cgState { cgStSwitchIndex = f (cgStSwitchIndex cgState)}

updateIfIndex :: (Int -> Int) -> CgState -> CgState
updateIfIndex f cgState = cgState { cgStIfIndex = f (cgStIfIndex cgState)}

updateLambdaIndex :: (Map.Map ClassName Int -> Map.Map ClassName Int) -> CgState -> CgState
updateLambdaIndex f cgState = cgState { cgStLambdaIndex = f (cgStLambdaIndex cgState)}

updateShouldDescribeFrame :: (Bool -> Bool) -> CgState -> CgState
updateShouldDescribeFrame f cgState = cgState { shouldDescribeFrame = f (shouldDescribeFrame cgState)}

updateFunctionArgs :: ([Name] -> [Name]) -> CgState -> CgState
updateFunctionArgs f cgState = cgState { cgStFunctionArgs = f (cgStFunctionArgs cgState)}

data CgWriter = CgWriter { instructions :: DL.DList Asm, deps :: DL.DList Asm }

instance Monoid CgWriter where
  mempty = CgWriter mempty mempty
  mappend (CgWriter ins1 deps1) (CgWriter ins2 deps2) = CgWriter (ins1 <> ins2) (deps1 <> deps2)

writeIns :: DL.DList Asm -> Cg ()
writeIns ins = tell $ CgWriter ins []

writeDeps :: DL.DList Asm -> Cg ()
writeDeps ds = tell $ CgWriter [] ds

type Cg a = RWS CgEnv CgWriter CgState a
