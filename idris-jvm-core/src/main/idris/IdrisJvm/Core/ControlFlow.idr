module IdrisJvm.Core.ControlFlow

import IdrisJvm.Core.Asm
import IdrisJvm.Core.Common
import IdrisJvm.Core.Constant
import IdrisJvm.IR.Types

%access public export

mutual
  cgSwitch : Lazy (Asm ()) -> (Lazy (Asm ()) -> SExp -> Asm ()) -> LVar -> List SAlt -> Asm ()
  cgSwitch ret cgBody e alts
    = if all isIntCase alts
        then do
          switchIndex <- FreshSwitchIndex
          let switch = csWithLbls switchIndex alts
          let switchVar = locIndex e
          let nonDefaultCases = filter (\(_, _, alt) => not (defaultCase alt)) switch
          let labels = (\(lbl, _, _) => lbl) <$> nonDefaultCases
          let exprs = catMaybes $ (\(_, expr, _) => expr) <$> nonDefaultCases
          let switchEndLbl = switchEndLabel switchIndex
          let hasDefault = any defaultCase alts
          let switchDefaultLbl = if hasDefault then defaultLabel switchIndex else switchEndLbl
          let isConstructorSwitch = any conCase alts
          let caseToAlt = \(label, _, alt) => cgAlt ret cgBody label switchIndex switchVar alt
          sequence_ $ (CreateLabel . fst) <$> switch
          CreateLabel switchEndLbl

          if isConstructorSwitch
            then switchConstructorExpr switchVar
            else switchIntExpr switchVar

          LookupSwitch switchDefaultLbl labels exprs
          sequence_ $ the (List (Asm ())) $ caseToAlt <$> switch
          LabelStart (switchEndLabel switchIndex)
          Frame FSame 0 [] 0 []
        else cgIf ret cgBody e nalts where
                f : SAlt -> SAlt -> Bool
                f (SDefaultCase _) (SDefaultCase _) = True
                f _ _                               = False
                nalts : List SAlt
                nalts = nubBy f alts

  cgIf : Lazy (Asm ()) -> (Lazy (Asm ()) -> SExp -> Asm ()) -> LVar -> List SAlt -> Asm ()
  cgIf ret cgBody e alts = do
       ifIndex <- FreshIfIndex
       let ifExpr = ifCasesWithLbls ifIndex alts
       let fallbackLabels = drop 1 ((\(lbl, _, _) => lbl) <$> ifExpr) ++ [ifEndLabel ifIndex]
       sequence_ $ CreateLabel . (\(lbl, _, _) => lbl) <$> ifExpr
       CreateLabel $ ifEndLabel ifIndex
       sequence_ $ gen ifIndex ret cgBody e <$> zip fallbackLabels ifExpr
       LabelStart (ifEndLabel ifIndex)
       Frame FSame 0 [] 0 []
     where
        gen : Nat
           -> Lazy (Asm ())
           -> (Lazy (Asm ()) -> SExp -> Asm ())
           -> LVar
           -> (String, String, Maybe (Asm ()), SAlt)
           -> Asm ()
        gen ifIndex ret cgBody e (nextLabel, label, ifExpr, expr)
          = maybe (cgElseCase ifIndex ret cgBody e nextLabel label expr)
                (\condition => cgIfElseIfCase ifIndex ret cgBody e nextLabel label condition expr)
                ifExpr

  cgIfElseIfCase : Nat
                -> Lazy (Asm ())
                -> (Lazy (Asm ()) -> SExp -> Asm ())
                -> LVar
                -> String
                -> String
                -> Asm ()
                -> SAlt
                -> Asm ()
  cgIfElseIfCase ifIndex ret cgBody e nextLabel label ifExpr (SConstCase _ expr) = do
    LabelStart label
    addFrame
    Aload $ locIndex e
    ifExpr
    InvokeMethod InvokeStatic utilClass "equals" "(Ljava/lang/Object;Ljava/lang/Object;)Z" False
    Ifeq nextLabel
    cgBody ret expr
    Goto $ ifEndLabel ifIndex

  cgElseCase : Nat
          -> Lazy (Asm ())
          -> (Lazy (Asm ()) -> SExp -> Asm ())
          -> LVar
          -> String
          -> String
          -> SAlt
          -> Asm ()
  cgElseCase ifIndex ret cgBody _ _ label (SDefaultCase expr) = do
    cgCase ret cgBody label expr
    Goto $ ifEndLabel ifIndex

  cgCase : Lazy (Asm ()) -> (Lazy (Asm ()) -> SExp -> Asm ()) -> Label -> SExp -> Asm ()
  cgCase ret cgBody label expr = do
    LabelStart label
    addFrame
    cgBody ret expr

  cgAltNonConCase : Lazy (Asm ()) -> (Lazy (Asm ()) -> SExp -> Asm ()) -> Label -> Nat -> SExp -> Asm ()
  cgAltNonConCase ret cgBody label si expr = do
    cgCase ret cgBody label expr
    Goto $ switchEndLabel si

  cgAlt : Lazy (Asm ()) -> (Lazy (Asm ()) -> SExp -> Asm ()) -> Label -> Nat -> Int -> SAlt -> Asm ()
  cgAlt ret cgBody label si _ (SConstCase _ expr) = cgAltNonConCase ret cgBody label si expr

  cgAlt ret cgBody label si _ (SDefaultCase expr) = cgAltNonConCase ret cgBody label si expr

  cgAlt ret cgBody label si sv (SConCase lv _ _ args expr) = do
      LabelStart label
      addFrame
      extractConParams lv
      cgBody ret expr

      Goto $ switchEndLabel si

    where
      project : Nat -> Nat -> Asm ()
      project i v = do
        idrisObjectProperty sv (cast i)
        Astore $ cast v

      argsLength : Nat
      argsLength = length args

      extractConParams : Int -> Asm ()
      extractConParams lv = case isLTE 1 argsLength of
        Yes prf => sequence_ $ (uncurry project) <$> List.zip (natRange 0 (argsLength - 1)) (natRange (cast lv) ((cast lv) + (argsLength - 1)))
        No contra => Pure ()

  conCase : SAlt -> Bool
  conCase (SConCase _ _ _ _ _) = True
  conCase _           = False

  defaultCase : SAlt -> Bool
  defaultCase (SDefaultCase _) = True
  defaultCase _                = False

  csWithLbls : Nat -> List SAlt -> List (String, Maybe Int, SAlt)
  csWithLbls si alts = sortBy compareCase $ List.zipWith label cs [0.. (length cs)] where

    cs : List (Maybe Int, SAlt)
    cs = zip (caseExpr <$> alts) alts

    label : (Maybe Int, SAlt) -> Nat -> (String, Maybe Int, SAlt)
    label (Just expr, alt) i = (labelName si i, Just expr, alt)
    label (Nothing, alt) _   = (defaultLabel si, Nothing, alt)

    compareCase (_, Just c1, _) (_, Just c2, _) = compare c1 c2
    compareCase (_, Just _, _) (_, Nothing, _)  = LT
    compareCase (_, Nothing, _) (_, Just _, _)  = GT
    compareCase _ _                             = EQ

  ifCasesWithLbls : Nat -> List SAlt -> List (String, Maybe (Asm ()), SAlt)
  ifCasesWithLbls si alts = sortBy compareCase $ zipWith label cs [0..(length cs)] where

    cs : List (Maybe (Asm ()), SAlt)
    cs = zip (ifCaseExpr <$> alts) alts

    label : (Maybe (Asm ()), SAlt) -> Nat -> (String, Maybe (Asm ()), SAlt)
    label (Just expr, alt) i = (ifLabelName si i, Just expr, alt)
    label (Nothing, alt) _   = (ifDefaultLabel si, Nothing, alt)

    compareCase (_, Just _, _) (_, Nothing, _) = LT
    compareCase (_, Nothing, _) (_, Just _, _) = GT
    compareCase _ _                            = EQ

  switchEndLabel : Nat -> String
  switchEndLabel switchIndex = "$switch" ++ show switchIndex ++ "$end"

  labelName : Nat -> Nat -> String
  labelName switchIndex labelIndex
    = "$switch" ++ show switchIndex ++ "$label" ++ show labelIndex

  defaultLabel : Nat -> String
  defaultLabel switchIndex = "$switch" ++ show switchIndex ++ "$defaultLabel"

  ifEndLabel : Nat -> String
  ifEndLabel ifIndex = "$if" ++ show ifIndex ++ "$end"

  ifLabelName : Nat -> Nat -> String
  ifLabelName ifIndex labelIndex = "$if" ++ show ifIndex ++ "$label" ++ show labelIndex

  ifDefaultLabel : Nat -> String
  ifDefaultLabel ifIndex = "$if" ++ show ifIndex ++ "$defaultLabel"

  switchConstructorExpr : Int -> Asm ()
  switchConstructorExpr varIndex = do
   Aload varIndex
   Checkcast idrisObjectType
   Field FGetField idrisObjectType "constructorId" "I"

  switchIntExpr : Int -> Asm ()
  switchIntExpr varIndex = do
    Aload varIndex
    InvokeMethod InvokeStatic utilClass "hash" "(Ljava/lang/Object;)I" False

  isIntCase : SAlt -> Bool
  isIntCase (SConCase _ _ _ _ _)    = True
  isIntCase (SDefaultCase _)        = True
  isIntCase (SConstCase (I _) _)    = True
  isIntCase (SConstCase (B8 _) _)   = True
  isIntCase (SConstCase (B16 _) _)  = True
  isIntCase (SConstCase (Ch _) _)   = True
  isIntCase (SConstCase TheWorld _) = True
  isIntCase (SConstCase x _)        = isTypeConst x
  isIntCase _                       = False

  caseExpr : SAlt -> Maybe Int
  caseExpr (SConstCase t _) = Just $ constCaseExpr t where
    constCaseExpr : Const -> Int
    constCaseExpr (I i) = i
    constCaseExpr (B8 i) = prim__zextB8_Int i
    constCaseExpr (B16 i) = prim__zextB16_Int i
    constCaseExpr (Ch c) = ord c
    constCaseExpr TheWorld = 0
    constCaseExpr x = if isTypeConst x then 0 else jerror $ "Constant " ++ show x ++ " cannot be compiled to 'Switch'."

  caseExpr (SConCase _ t _ _ _) = Just t
  caseExpr (SDefaultCase _) = Nothing

  ifCaseExpr : SAlt -> Maybe (Asm ())
  ifCaseExpr (SConstCase t _) = Just $ constCaseExpr t where
    constCaseExpr : Const -> Asm ()
    constCaseExpr (I x) = jerror $ "Constant int expected to be compiled to 'switch'"
    constCaseExpr (BI i) = newBigInteger i
    constCaseExpr (Fl x) = jerror $ "Constant float expected to be compiled to 'switch'"
    constCaseExpr (Ch x) = jerror $ "Constant char expected to be compiled to 'switch'"
    constCaseExpr (Str s) = Ldc $ StringConst s
    constCaseExpr (B8 x) = jerror $ "Constant b8 expected to be compiled to 'switch'"
    constCaseExpr (B16 x) = jerror $ "Constant b16 expected to be compiled to 'switch'"
    constCaseExpr (B32 x) = jerror $ "Constant b32 cannot be compiled to 'if' yet"
    constCaseExpr (B64 x) = jerror $ "Constant b64 cannot be compiled to 'if' yet"
    constCaseExpr (AType x) = jerror $ "Constant AType cannot be compiled to 'if' yet"
    constCaseExpr StrType = jerror "Constant StrType cannot be compiled to 'if' yet"
    constCaseExpr WorldType = jerror "Constant WorldType cannot be compiled to 'if' yet"
    constCaseExpr TheWorld = jerror "Constant TheWorld cannot be compiled to 'if' yet"
    constCaseExpr VoidType = jerror "Constant VoidType cannot be compiled to 'if' yet"
    constCaseExpr Forgot = jerror "Constant Forgot cannot be compiled to 'if' yet"
  ifCaseExpr _ = Nothing

cgIfElse : Lazy (Asm ())
        -> (Lazy (Asm ()) -> SExp -> Asm ())
        -> LVar
        -> (Label -> Asm ())
        -> Maybe Int
        -> SExp
        -> SExp
        -> Asm ()
cgIfElse ret cgBody e condition valueStore case1 case2 = do
    ifIndex <- FreshIfIndex
    let ifLabel = ifLabelName ifIndex 0
    let elseLabel = ifLabelName ifIndex 1
    let endLabel = ifEndLabel ifIndex
    CreateLabel ifLabel
    CreateLabel elseLabel
    CreateLabel endLabel
    Aload $ locIndex e
    condition elseLabel
    LabelStart ifLabel
    maybe (pure ()) store valueStore
    cgBody ret case1
    Goto endLabel
    LabelStart elseLabel
    addFrame
    cgBody ret case2
    LabelStart endLabel
    Frame FSame 0 [] 0 []
  where
    store : Int -> Asm ()
    store loc = do
       Aload $ locIndex e
       Astore loc

cgIfNonNull : Lazy (Asm ()) -> (Lazy (Asm ()) -> SExp -> Asm ()) -> LVar -> Int -> SExp -> SExp -> Asm ()
cgIfNonNull ret cgBody e loc ifExp elseExp = cgIfElse ret cgBody e Ifnull (Just loc) ifExp elseExp

cgIfNull : Lazy (Asm ()) -> (Lazy (Asm ()) -> SExp -> Asm ()) -> LVar -> SExp -> SExp -> Asm ()
cgIfNull ret cgBody e ifExp elseExp = cgIfElse ret cgBody e Ifnonnull Nothing ifExp elseExp

cgIfTrueElse : Lazy (Asm ()) -> (Lazy (Asm ()) -> SExp -> Asm ()) -> LVar -> SExp -> SExp -> Asm ()
cgIfTrueElse ret cgBody e ifExp elseExp = cgIfElse ret cgBody e condition Nothing ifExp elseExp where
  condition : Label -> Asm ()
  condition elseLabel = do
    Checkcast "java/lang/Boolean"
    unboxBool
    Ifeq elseLabel
