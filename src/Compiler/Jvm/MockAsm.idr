module Compiler.Jvm.MockAsm

import Compiler.Common
import Compiler.CompileExpr
import Compiler.Inline

import Core.Context
import Core.Name
import Core.TT

import Data.Maybe
import Libraries.Data.SortedMap
import Data.String
import Data.Vect

import Compiler.Jvm.Asm
import Compiler.Jvm.InferredType
import Compiler.Jvm.Jname
import Compiler.Jvm.ShowUtil

%foreign "jvm:toString(java/lang/Object java/lang/String),java/util/Objects"
prim_objectToString : AnyPtr -> PrimIO String

export
objectToString : a -> String
objectToString value = unsafePerformIO $ primIO (prim_objectToString (believe_me value))

log : String -> IO ()
log message = do
  time <- currentTimeString
  threadName <- getCurrentThreadName
  putStrLn $ time ++ " [" ++ threadName ++ "]" ++ message

export
mockRunAsm : AsmState -> Asm a -> IO (a, AsmState)
mockRunAsm state Aaload = assemble state $ log "aaload"
mockRunAsm state Aastore = assemble state $ log "aastore"
mockRunAsm state Aconstnull = assemble state $ log "aconstnull"
mockRunAsm state (Aload n) = assemble state $
    log $ "aload " ++ show n
mockRunAsm state (Anewarray desc) = assemble state $
    log $ "anewarray " ++ desc
mockRunAsm state Anewintarray     = assemble state $
    log "anewintarray"
mockRunAsm state Anewbooleanarray = assemble state $
    log "anewbooleanarray"
mockRunAsm state Anewbytearray    = assemble state $
    log "anewbytearray"
mockRunAsm state Anewchararray    = assemble state $
    log "anewchararray"
mockRunAsm state Anewshortarray   = assemble state $
    log "anewshortarray"
mockRunAsm state Anewlongarray    = assemble state $
    log "anewlongarray"
mockRunAsm state Anewfloatarray   = assemble state $
    log "anewfloatarray"
mockRunAsm state Anewdoublearray  = assemble state $
    log "anewdoublearray"
mockRunAsm state Arraylength      = assemble state $ log "arraylength"
mockRunAsm state Areturn          = assemble state $ log "areturn"
mockRunAsm state (Astore n)       = assemble state $
    log $ "astore " ++ show n
mockRunAsm state Baload           = assemble state $ log "baload"
mockRunAsm state Bastore          = assemble state $ log "bastore"
mockRunAsm state Caload           = assemble state $ log "caload"
mockRunAsm state Castore          = assemble state $ log "castore"
mockRunAsm state (Checkcast desc) = assemble state $
    log $ "checkcast " ++ desc
mockRunAsm state (ClassCodeStart version access className sig parent intf anns) = assemble state $
  log $ unwords [
    "classCodeStart",
    show version,
    show (show access),
    className,
    (fromMaybe "" sig),
    parent]

mockRunAsm state (CreateClass opts) = assemble state $ log $ "createClass " ++ show opts
mockRunAsm state (CreateField accs sourceFileName className fieldName desc sig fieldInitialValue annotations) =
  assemble state $ do
    let jaccs = sum $ accessNum <$> accs
    log $ unwords [
      "createField",
      show jaccs,
      sourceFileName,
      className,
      fieldName,
      desc,
      fromMaybe "" sig,
      (objectToString $ maybeToNullable (toJFieldInitialValue <$> fieldInitialValue))]

mockRunAsm state (CreateLabel label) = assemble state $ pure ()

mockRunAsm state (CreateMethod accs sourceFileName className methodName desc sig exceptions anns paramAnns) =
    let newState = { currentMethodName := Jqualified className methodName } state
    in assemble newState $ do
        log $ "**************** " ++ methodName ++ " ******************"
        log $ unwords [
            "createMethod",
            show accs,
            sourceFileName,
            className,
            methodName,
            desc]

mockRunAsm state (CreateIdrisConstructorClass className isStringConstructor constructorParameterCount) =
    assemble state $ log ("CreateIdrisConstructorClass " ++ className ++ " " ++
        show isStringConstructor ++ " " ++ show constructorParameterCount)

mockRunAsm state D2i = assemble state $ log "d2i"
mockRunAsm state D2f = assemble state $ log "d2f"
mockRunAsm state D2l = assemble state $ log "d2l"
mockRunAsm state Dadd = assemble state $ log "dadd"
mockRunAsm state Dcmpl = assemble state $ log "dcmpl"
mockRunAsm state Dcmpg = assemble state $ log "dcmpg"
mockRunAsm state (Dconst n) = assemble state $ log $ "dconst " ++ show n
mockRunAsm state Daload = assemble state $ log "daload"
mockRunAsm state Dastore = assemble state $ log "dastore"
mockRunAsm state Ddiv = assemble state $ log "ddiv"

mockRunAsm state (Debug message) = assemble state $ log message

mockRunAsm state (Dload n) =
    assemble state $ log $ "dload " ++ show n
mockRunAsm state Dmul = assemble state $ log "dmul"
mockRunAsm state Dneg = assemble state $ log "dneg"
mockRunAsm state Drem = assemble state $ log "drem"
mockRunAsm state Dreturn = assemble state $ log "dreturn"
mockRunAsm state (Dstore n) =
    assemble state $ log $ "dstore " ++ show n
mockRunAsm state Dsub = assemble state $ log "dsub"
mockRunAsm state Dup = assemble state $ log "dup"
mockRunAsm state (Error err) =
    assemble state $ log $ "error " ++ err
mockRunAsm state F2d = assemble state $ log "f2d"
mockRunAsm state Faload = assemble state $ log "faload"
mockRunAsm state Fastore = assemble state $ log "fastore"
mockRunAsm state (Fconst n) =
    assemble state $ log $ "fconst " ++ show n
mockRunAsm state (Field finsType cname fname desc) = assemble state $ do
  let finsTypeNum = fieldInsTypeNum finsType
  log $ unwords [
    "field",
    show finsTypeNum,
    cname,
    fname,
    desc]

mockRunAsm state FieldEnd = assemble state $ log "fieldEnd"

mockRunAsm state (Fload n) =
    assemble state $ log $ "fload " ++ show n

mockRunAsm state (Frame frameType nLocal localSigs nStack stackSigs) = assemble state $ do
  let ftypeNum = frameTypeNum frameType
  log $ unwords [
    "frame",
    show ftypeNum,
    show nLocal,
    show nStack]

mockRunAsm state Freturn = assemble state $ log "freturn"
mockRunAsm state (Fstore n) =
    assemble state $ log $ "fstore " ++ show n

mockRunAsm state (Goto label) =
    assemble state $ log $ "goto " ++ label

mockRunAsm state I2b = assemble state $ log "i2b"
mockRunAsm state I2c = assemble state $ log "i2c"
mockRunAsm state I2d = assemble state $ log "i2d"
mockRunAsm state I2l = assemble state $ log "i2l"
mockRunAsm state I2s = assemble state $ log "i2s"
mockRunAsm state Iadd = assemble state $ log "iadd"
mockRunAsm state Iaload = assemble state $ log "iaload"
mockRunAsm state Iand = assemble state $ log "iand"
mockRunAsm state Iastore = assemble state $ log "iastore"
mockRunAsm state Ior = assemble state $ log "ior"
mockRunAsm state Ixor = assemble state $ log "ixor"
mockRunAsm state Icompl = assemble state $ log "icompl"
mockRunAsm state (Iconst n) = assemble state $ log $ "iconst " ++ show n
mockRunAsm state Idiv = assemble state $ log "idiv"
mockRunAsm state (Ifeq label) =
    assemble state $ log $ "ifeq " ++ label
mockRunAsm state (Ifge label) =
    assemble state $ log $ "ifge " ++ label
mockRunAsm state (Ifgt label) =
    assemble state $ log $ "ifgt " ++ label
mockRunAsm state (Ificmpeq label) =
    assemble state $ log $ "ificmpeq " ++ label
mockRunAsm state (Ificmpge label) =
    assemble state $ log $ "ificmpge " ++ label
mockRunAsm state (Ificmpgt label) =
    assemble state $ log $ "ificmpgt " ++ label
mockRunAsm state (Ificmple label) =
    assemble state $ log $ "ificmple " ++ label
mockRunAsm state (Ificmplt label) =
    assemble state $ log $ "ificmplt " ++ label
mockRunAsm state (Ifacmpne label) =
    assemble state $ log $ "ifacmpne " ++ label
mockRunAsm state (Ificmpne label) =
    assemble state $ log $ "ificmpne " ++ label
mockRunAsm state (Ifle label) =
    assemble state $ log $ "ifle " ++ label
mockRunAsm state (Iflt label) =
    assemble state $ log $ "iflt " ++ label
mockRunAsm state (Ifne label) =
    assemble state $ log $ "ifne " ++ label
mockRunAsm state (Ifnonnull label) =
    assemble state $ log $ "ifnonnull " ++ label
mockRunAsm state (Ifnull label) =
    assemble state $ log $ "ifnull " ++ label
mockRunAsm state (Iload n) =
    assemble state $ log $ "iload " ++ show n
mockRunAsm state Imul = assemble state $ log "imul"
mockRunAsm state Ineg = assemble state $ log "ineg"
mockRunAsm state (InstanceOf className) =
    assemble state $ log $ "instanceOf " ++ className
mockRunAsm state (InvokeMethod invocType cname mname desc isIntf) = assemble state $ do
  log $ unwords [
    "invokeMethod",
    show invocType,
    cname,
    mname,
    desc,
    show isIntf]

mockRunAsm state (InvokeDynamic mname desc handle bsmArgs) = assemble state $ do
  jbsmArgs <- sequence $ toJbsmArg <$> bsmArgs
  jhandle <- toJHandle handle
  log $ unwords [
    "invokeDynamic",
    mname,
    desc,
    (objectToString $ the Object $ believe_me jhandle),
    (objectToString $ the Object $ believe_me jbsmArgs)]

mockRunAsm state Irem = assemble state $ log "irem"
mockRunAsm state Ireturn = assemble state $ log "ireturn"
mockRunAsm state Ishl = assemble state $ log "ishl"
mockRunAsm state Ishr = assemble state $ log "ishr"
mockRunAsm state (Istore n) = assemble state $ log $ "istore " ++ show n
mockRunAsm state Isub = assemble state $ log "isub"
mockRunAsm state Iushr = assemble state $ log "iushr"
mockRunAsm state L2d = assemble state $ log "l2d"
mockRunAsm state L2i = assemble state $ log "l2i"
mockRunAsm state (LabelStart label) = assemble state $ log (label ++ ":")
mockRunAsm state Ladd = assemble state $ log "ladd"
mockRunAsm state Laload = assemble state $ log "laload"
mockRunAsm state Land = assemble state $ log "land"
mockRunAsm state Lastore = assemble state $ log "lastore"
mockRunAsm state Lcmp = assemble state $ log "lcmp"
mockRunAsm state Lcompl = assemble state $ log "lcompl"

mockRunAsm state (Ldc (TypeConst ty)) =
    assemble state $ log $ "ldcType " ++ ty
mockRunAsm state (Ldc constant) = assemble state $ do
    log ("ldc " ++ (objectToString (constantToObject constant)))

mockRunAsm state Ldiv = assemble state $ log "ldiv"

mockRunAsm state (LineNumber lineNumber label) = assemble state $
    log $ unwords [
        "lineNumber",
        show lineNumber,
        label]

mockRunAsm state (Lload n) = assemble state $
    log $ "lload " ++ show n
mockRunAsm state Lmul = assemble state $ log "lmul"
mockRunAsm state Lneg = assemble state $ log "lneg"

mockRunAsm state (LocalVariable name descriptor signature startLabel endLabel index) = assemble state $
    log $ unwords [
        "localVariable",
        name,
        descriptor,
        (fromMaybe "null" signature),
        startLabel,
        endLabel,
        show index]
mockRunAsm state (LookupSwitch defaultLabel labels cases) = assemble state $ do
  let jcases = integerValueOf <$> cases
  log $ unwords [
    "lookupSwitch",
    defaultLabel,
    (objectToString (the Object $ believe_me labels)),
    (objectToString (the Object $ believe_me jcases))]

mockRunAsm state Lor = assemble state $ log "lor"

mockRunAsm state Lrem = assemble state $ log "lrem"
mockRunAsm state Lreturn = assemble state $ log "lreturn"
mockRunAsm state Lshl = assemble state $ log "lshl"
mockRunAsm state Lshr = assemble state $ log "lshr"
mockRunAsm state (Lstore n) = assemble state $
    log $ "lstore " ++ show n
mockRunAsm state Lsub = assemble state $ log "lsub"
mockRunAsm state Lushr = assemble state $ log "lushr"
mockRunAsm state Lxor = assemble state $ log "lxor"
mockRunAsm state (MaxStackAndLocal stack local) = assemble state $
    log $ "maxStackAndLocal " ++ show stack ++ " " ++ show local
mockRunAsm state MethodCodeStart = assemble state $
    log "methodCodeStart"
mockRunAsm state MethodCodeEnd = assemble state $ do
    log "methodCodeEnd"
    log $ "**********************************"
mockRunAsm state (Multianewarray desc dims) = assemble state $
    log $ unwords ["multiANewArray", desc, show dims]
mockRunAsm state (New cname) = assemble state $
    log $ "asmNew " ++ cname
mockRunAsm state Pop = assemble state $ log "pop"
mockRunAsm state Pop2 = assemble state $ log "pop2"
mockRunAsm state Return = assemble state $ log "voidReturn"
mockRunAsm state Saload = assemble state $ log "saload"
mockRunAsm state Sastore = assemble state $ log "sastore"
mockRunAsm state (SourceInfo sourceFileName)
  = assemble state $ log $ "sourceInfo " ++ sourceFileName
mockRunAsm state (LiftIo action) = assemble state action

mockRunAsm state (Throw fc message) = pure (believe_me $ crash $ show fc ++ ": " ++ message, state)
mockRunAsm state GetState = pure (state, state)
mockRunAsm state (SetState newState) = pure ((), newState)

mockRunAsm st (Pure value) = pure (value, st)
mockRunAsm st (Bind action f) = do
  (result, nextSt) <- mockRunAsm st action
  mockRunAsm nextSt $ f result
