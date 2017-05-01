{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module IdrisJvm.Codegen.ControlFlow where

import           Data.Char                  (ord)
import qualified Data.DList                 as DL
import           Data.Int
import           Data.List                  (nubBy, sortBy)
import           Data.Maybe
import           Idris.Core.TT
import           IdrisJvm.Codegen.Assembler
import           IdrisJvm.Codegen.Common
import           IdrisJvm.Codegen.Types
import           IRTS.Lang
import           IRTS.Simplified

cgSwitch :: Cg () -> (Cg () -> SExp -> Cg ()) -> LVar -> [SAlt] -> Cg ()
cgSwitch ret cgBody e alts | all isIntCase alts = do
    switchIndex <- freshSwitchIndex
    let switch = csWithLbls switchIndex alts
        switchVar = locIndex e
        nonDefaultCases = filter (\(_, _, alt) -> not (defaultCase alt)) switch
        labels = (\(lbl, _, _) -> lbl) <$> nonDefaultCases
        exprs = catMaybes $ (\(_, expr, _) -> expr) <$> nonDefaultCases
        caseIns = mapM_ (\(label, _, alt) -> cgAlt ret cgBody label switchIndex switchVar alt) switch
        switchEndLbl = switchEndLabel switchIndex
        switchDefaultLbl = if hasDefault then defaultLabel switchIndex else switchEndLbl
        hasDefault = any defaultCase alts
        isConstructorSwitch = any conCase alts

    writeIns $ DL.fromList (CreateLabel . (\(lbl, _, _) -> lbl) <$> switch)
    writeIns [ CreateLabel switchEndLbl ]

    if isConstructorSwitch
      then switchConstructorExpr switchVar
      else switchIntExpr switchVar

    writeIns [ LookupSwitch switchDefaultLbl labels exprs ]
    caseIns
    writeIns [ LabelStart (switchEndLabel switchIndex)
             , Frame FSame 0 [] 0 []]

cgSwitch ret cgBody e alts = cgIf ret cgBody e nalts where
  nalts = nubBy f alts
  f SDefaultCase{} SDefaultCase{} = True
  f _ _                           = False

cgIf :: Cg () -> (Cg () -> SExp -> Cg ()) -> LVar -> [SAlt] -> Cg ()
cgIf ret cgBody e alts = do
  ifIndex <- freshIfIndex
  let ifExpr = ifCasesWithLbls ifIndex alts
      fallbackLabels = drop 1 ((\(lbl, _, _) -> lbl) <$> ifExpr) ++ [ifEndLabel ifIndex]
  writeIns $ DL.fromList (CreateLabel . (\(lbl, _, _) -> lbl) <$> ifExpr)
  writeIns [ CreateLabel $ ifEndLabel ifIndex ]
  mapM_ (uncurry $ cgIfCase ifIndex ret cgBody e) $ zip fallbackLabels ifExpr
  writeIns [ LabelStart (ifEndLabel ifIndex)
           , Frame FSame 0 [] 0 []]

cgIfCase :: Int -> Cg () -> (Cg () -> SExp -> Cg ()) -> LVar -> String -> (String, Maybe (Cg ()), SAlt) -> Cg ()
cgIfCase ifIndex ret cgBody e nextLabel (label, Just ifExpr, SConstCase _ expr) = do
  writeIns [ LabelStart label ]
  addFrame
  writeIns [Aload $ locIndex e ]
  ifExpr
  writeIns [ InvokeMethod InvokeStatic (rtClassSig "Util") "equals" "(Ljava/lang/Object;Ljava/lang/Object;)Z" False
           , Ifeq nextLabel
           ]
  cgBody ret expr
  writeIns [ Goto $ ifEndLabel ifIndex ]

cgIfCase ifIndex ret cgBody _ _ (label, Nothing, SDefaultCase expr) = do
  cgCase ret cgBody label expr
  writeIns [ Goto $ ifEndLabel ifIndex ]

cgIfCase _ _ _ _ _ _ = error "Unexpected if expression"

cgCase :: Cg () -> (Cg () -> SExp -> Cg ()) -> Label -> SExp -> Cg ()
cgCase ret cgBody label expr = do
  writeIns [ LabelStart label ]
  addFrame
  cgBody ret expr

cgAltNonConCase :: Cg () -> (Cg () -> SExp -> Cg ()) -> Label -> Int -> SExp -> Cg ()
cgAltNonConCase ret cgBody label si expr = do
  cgCase ret cgBody label expr
  writeIns [ Goto $ switchEndLabel si ]

cgAlt :: Cg () -> (Cg () -> SExp -> Cg ()) -> Label -> Int -> Int -> SAlt -> Cg ()
cgAlt ret cgBody label si _ (SConstCase _ expr) = cgAltNonConCase ret cgBody label si expr

cgAlt ret cgBody label si _ (SDefaultCase expr) = cgAltNonConCase ret cgBody label si expr

cgAlt ret cgBody label si sv (SConCase lv _ _ args expr) = do
    writeIns [ LabelStart label ]
    addFrame
    extractConParams
    cgBody ret expr

    writeIns [ Goto $ switchEndLabel si ]

   where
     project i v =
       writeIns [ Aload sv
                , Checkcast "[Ljava/lang/Object;"
                , Iconst i
                , Aaload
                , Astore v
                ]

     extractConParams =
       mapM_ (uncurry project) $ zip [1..] [lv .. (lv + length args - 1)]

conCase :: SAlt -> Bool
conCase SConCase {} = True
conCase _           = False

defaultCase :: SAlt -> Bool
defaultCase (SDefaultCase _) = True
defaultCase _                = False

csWithLbls :: Int -> [SAlt] -> [(String, Maybe Int32, SAlt)]
csWithLbls si alts = sortBy compareCase $ zipWith label cs [0..] where

  cs :: [(Maybe Int32, SAlt)]
  cs = zip (caseExpr <$> alts) alts

  label :: (Maybe Int32, SAlt) -> Int -> (String, Maybe Int32, SAlt)
  label (Just expr, alt) i = (labelName si i, Just expr, alt)
  label (Nothing, alt) _   = (defaultLabel si, Nothing, alt)

  compareCase (_, Just c1, _) (_, Just c2, _) = compare c1 c2
  compareCase (_, Just _, _) (_, Nothing, _)  = LT
  compareCase (_, Nothing, _) (_, Just _, _)  = GT
  compareCase _ _                             = EQ

ifCasesWithLbls :: Int -> [SAlt] -> [(String, Maybe (Cg ()), SAlt)]
ifCasesWithLbls si alts = sortBy compareCase $ zipWith label cs [0..] where

  cs :: [(Maybe (Cg ()), SAlt)]
  cs = zip (ifCaseExpr <$> alts) alts

  label :: (Maybe (Cg ()), SAlt) -> Int -> (String, Maybe (Cg ()), SAlt)
  label (Just expr, alt) i = (ifLabelName si i, Just expr, alt)
  label (Nothing, alt) _   = (ifDefaultLabel si, Nothing, alt)

  compareCase (_, Just _, _) (_, Nothing, _) = LT
  compareCase (_, Nothing, _) (_, Just _, _) = GT
  compareCase _ _                            = EQ

switchEndLabel :: Int -> String
switchEndLabel switchIndex = "$switch" ++ show switchIndex ++ "$end"

labelName :: Int -> Int -> String
labelName switchIndex labelIndex
  = "$switch" ++ show switchIndex ++ "$label" ++ show labelIndex

defaultLabel :: Int -> String
defaultLabel switchIndex = "$switch" ++ show switchIndex ++ "$defaultLabel"

ifEndLabel :: Int -> String
ifEndLabel ifIndex = "$if" ++ show ifIndex ++ "$end"

ifLabelName :: Int -> Int -> String
ifLabelName ifIndex labelIndex
  = "$if" ++ show ifIndex ++ "$label" ++ show labelIndex

ifDefaultLabel :: Int -> String
ifDefaultLabel ifIndex = "$if" ++ show ifIndex ++ "$defaultLabel"


switchConstructorExpr :: Int -> Cg ()
switchConstructorExpr varIndex
 = writeIns [ Aload varIndex
            , InvokeMethod InvokeStatic (rtClassSig "Runtime") "constructorIndex" "(Ljava/lang/Object;)I" False
            ]

switchIntExpr :: Int -> Cg ()
switchIntExpr varIndex
  = writeIns [ Aload varIndex
             , InvokeMethod InvokeStatic (rtClassSig "Util") "hash" "(Ljava/lang/Object;)I" False
             ]

isIntCase :: SAlt -> Bool
isIntCase SConCase{}              = True
isIntCase SDefaultCase{}          = True
isIntCase (SConstCase (I _) _)    = True
isIntCase (SConstCase (B8 _) _)   = True
isIntCase (SConstCase (B16 _) _)  = True
isIntCase (SConstCase (Ch _) _)   = True
isIntCase (SConstCase TheWorld _) = True
isIntCase (SConstCase x _)        | isTypeConst x = True
isIntCase _                       = False

caseExpr :: SAlt -> Maybe Int32
caseExpr (SConstCase t _) = Just $ constCaseExpr t where
  constCaseExpr :: Const -> Int32
  constCaseExpr (I i) = fromIntegral i
  constCaseExpr (B8 i) = fromIntegral i
  constCaseExpr (B16 i) = fromIntegral i
  constCaseExpr (Ch c) = fromIntegral $ ord c
  constCaseExpr TheWorld = 0
  constCaseExpr x | isTypeConst x = 0
  constCaseExpr x = error $ "Constant " ++ show x ++ " cannot be compiled to 'Switch'."

caseExpr (SConCase _ t _ _ _) = Just $ fromIntegral t
caseExpr (SDefaultCase _) = Nothing

ifCaseExpr :: SAlt -> Maybe (Cg ())
ifCaseExpr (SConstCase t _) = Just $ constCaseExpr t where
  constCaseExpr :: Const -> Cg ()
  constCaseExpr (I x) = error $ "Constant int " ++ show x ++ " expected to be compiled to 'switch'"
  constCaseExpr (BI i) = newBigInteger i
  constCaseExpr (Fl x) = error $ "Constant float " ++ show x ++ " expected to be compiled to 'switch'"
  constCaseExpr (Ch x) = error $ "Constant char " ++ show x ++ " expected to be compiled to 'switch'"
  constCaseExpr (Str s) = writeIns [Ldc $ StringConst s]
  constCaseExpr (B8 x) = error $ "Constant b8 " ++ show x ++ " expected to be compiled to 'switch'"
  constCaseExpr (B16 x) = error $ "Constant b16" ++ show x ++ " expected to be compiled to 'switch'"
  constCaseExpr (B32 x) = error $ "Constant b32 " ++ show x ++ " cannot be compiled to 'if' yet"
  constCaseExpr (B64 x) = error $ "Constant b64 " ++ show x ++ " cannot be compiled to 'if' yet"
  constCaseExpr (AType x) = error $ "Constant AType " ++ show x ++ " cannot be compiled to 'if' yet"
  constCaseExpr StrType = error "Constant StrType cannot be compiled to 'if' yet"
  constCaseExpr WorldType = error "Constant WorldType cannot be compiled to 'if' yet"
  constCaseExpr TheWorld = error "Constant TheWorld cannot be compiled to 'if' yet"
  constCaseExpr VoidType = error "Constant VoidType cannot be compiled to 'if' yet"
  constCaseExpr Forgot = error "Constant Forgot cannot be compiled to 'if' yet"
ifCaseExpr _ = Nothing
