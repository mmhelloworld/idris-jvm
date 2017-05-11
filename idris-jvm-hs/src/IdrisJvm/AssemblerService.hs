{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module IdrisJvm.AssemblerService where

import           Control.Lens               ((^.))
import           Data.Aeson
import qualified Data.Aeson                 as A (decode)
import           Data.Aeson.Types           (typeMismatch)
import           Data.Maybe
import           Data.Monoid                ((<>))
import           IdrisJvm.Codegen.Assembler (Asm)
import qualified Network.Wreq               as W
import           System.Directory           (getHomeDirectory)
import           System.Environment         (lookupEnv)
import           System.FilePath            ((</>))
import           Text.Read                  (readMaybe)

data AsmResponse = AsmResponse {-isSuccess ::-}Bool {-message ::-}String

instance FromJSON AsmResponse where
  parseJSON (Object v) = AsmResponse <$>
                         v .: "success" <*>
                         v .: "message"
  parseJSON invalid    = typeMismatch "AsmResponse" invalid


runAssembler :: [Asm] -> IO (Maybe String)
runAssembler instructions = do
  let assemblerRequest = object ["instructions" .= toJSON instructions]
  port <- getAsmPort
  asmResponse <- W.post (asmEndpoint port) assemblerRequest
  return $ asmResponseMessage $ A.decode (asmResponse ^. W.responseBody)

asmEndpoint :: Int -> String
asmEndpoint port = "http://localhost:" ++ show port ++ "/assembler/assemble"

getAsmPort :: IO Int
getAsmPort = do
  workingDir <- getWorkingDir
  asmPort <- readFile $ workingDir </> ".assembler"
  let err = "Could not get assembler server port. " <>
            "Please check whether jvm-assembler server is running."
  maybe (error err) pure $ readMaybe asmPort

getWorkingDir :: IO FilePath
getWorkingDir = do
  defaultWorkingDir <- (</> ".idrisjvm") <$> getHomeDirectory
  fromMaybe defaultWorkingDir <$> lookupEnv "IDRIS_JVM_WORK_DIR"

asmResponseMessage :: Maybe AsmResponse -> Maybe String
asmResponseMessage (Just (AsmResponse True _)) = Nothing
asmResponseMessage (Just (AsmResponse False msg)) = Just msg
asmResponseMessage _ = Just "Bytecode generation failed! Please check assembler server logs"
