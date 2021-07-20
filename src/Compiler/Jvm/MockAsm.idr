module Compiler.Jvm.MockAsm 

import Compiler.Common
import Compiler.CompileExpr
import Compiler.Inline

import Core.Context
import Core.Name
import Core.TT

import Data.Maybe
import Data.SortedMap
import Data.Strings
import Data.Vect

import Compiler.Jvm.Asm
import Compiler.Jvm.InferredType
import Compiler.Jvm.Jname
import Compiler.Jvm.ShowUtil

objectToString : Object -> IO String
objectToString obj = jvmStatic String "java/util/Objects.toString" [obj]

export
mockRunAsm : AsmState -> Asm a -> IO (a, AsmState)
mockRunAsm state Aaload = assemble state $ putStrLn "aaload"
mockRunAsm state Aastore = assemble state $ putStrLn "aastore"
mockRunAsm state Aconstnull = assemble state $ putStrLn "aconstnull"
mockRunAsm state (Aload n) = assemble state $
    putStrLn $ "aload " ++ show n
mockRunAsm state (Anewarray desc) = assemble state $
    putStrLn $ "anewarray " ++ desc
mockRunAsm state Anewintarray     = assemble state $
    putStrLn "anewintarray"
mockRunAsm state Anewbooleanarray = assemble state $
    putStrLn "anewbooleanarray"
mockRunAsm state Anewbytearray    = assemble state $
    putStrLn "anewbytearray"
mockRunAsm state Anewchararray    = assemble state $
    putStrLn "anewchararray"
mockRunAsm state Anewshortarray   = assemble state $
    putStrLn "anewshortarray"
mockRunAsm state Anewlongarray    = assemble state $
    putStrLn "anewlongarray"
mockRunAsm state Anewfloatarray   = assemble state $
    putStrLn "anewfloatarray"
mockRunAsm state Anewdoublearray  = assemble state $
    putStrLn "anewdoublearray"
mockRunAsm state Arraylength      = assemble state $ putStrLn "arraylength"
mockRunAsm state Areturn          = assemble state $ putStrLn "areturn"
mockRunAsm state (Astore n)       = assemble state $
    putStrLn $ "astore " ++ show n
mockRunAsm state Baload           = assemble state $ putStrLn "baload"
mockRunAsm state Bastore          = assemble state $ putStrLn "bastore"
mockRunAsm state Caload           = assemble state $ putStrLn "caload"
mockRunAsm state Castore          = assemble state $ putStrLn "castore"
mockRunAsm state (Checkcast desc) = assemble state $
    putStrLn $ "checkcast " ++ desc
mockRunAsm state (ClassCodeStart version access className sig parent intf anns) = assemble state $
  putStrLn $ unwords [
    "classCodeStart",
    show version,
    show (accessNum access),
    className,
    (fromMaybe "" sig),
    parent]

mockRunAsm state (CreateClass opts) = assemble state $ putStrLn $ "createClass " ++ show opts
mockRunAsm state (CreateField accs className fieldName desc sig fieldInitialValue) = assemble state $ do
  let jaccs = sum $ accessNum <$> accs
  putStrLn $ unwords [
    "createField",
    show jaccs,
    className,
    fieldName,
    desc,
    fromMaybe "" sig,
    !(objectToString $ maybeToNullable (toJFieldInitialValue <$> fieldInitialValue))]

mockRunAsm state (CreateLabel label) = assemble state $ pure label

mockRunAsm state (CreateMethod accs sourceFileName className methodName desc sig exceptions anns paramAnns) =
    let newState = record { currentMethodName = Jqualified className methodName } state
    in assemble newState $ do
        putStrLn $ "**************** " ++ methodName ++ " ******************"
        putStrLn $ unwords [
            "createMethod",
            show accs,
            sourceFileName,
            className,
            methodName,
            desc]

mockRunAsm state (CreateIdrisConstructorClass className isStringConstructor constructorParameterCount) =
    assemble state $ putStrLn ("CreateIdrisConstructorClass " ++ className ++ " " ++
        show isStringConstructor ++ " " ++ show constructorParameterCount)

mockRunAsm state D2i = assemble state $ putStrLn "d2i"
mockRunAsm state D2f = assemble state $ putStrLn "d2f"
mockRunAsm state Dadd = assemble state $ putStrLn "dadd"
mockRunAsm state Dcmpl = assemble state $ putStrLn "dcmpl"
mockRunAsm state Dcmpg = assemble state $ putStrLn "dcmpg"
mockRunAsm state (Dconst n) = assemble state $ putStrLn $ "dconst " ++ show n
mockRunAsm state Daload = assemble state $ putStrLn "daload"
mockRunAsm state Dastore = assemble state $ putStrLn "dastore"
mockRunAsm state Ddiv = assemble state $ putStrLn "ddiv"

mockRunAsm state (Debug message) = assemble state $ putStrLn message

mockRunAsm state (Dload n) =
    assemble state $ putStrLn $ "dload " ++ show n
mockRunAsm state Dmul = assemble state $ putStrLn "dmul"
mockRunAsm state Dneg = assemble state $ putStrLn "dneg"
mockRunAsm state Drem = assemble state $ putStrLn "drem"
mockRunAsm state Dreturn = assemble state $ putStrLn "dreturn"
mockRunAsm state (Dstore n) =
    assemble state $ putStrLn $ "dstore " ++ show n
mockRunAsm state Dsub = assemble state $ putStrLn "dsub"
mockRunAsm state Dup = assemble state $ putStrLn "dup"
mockRunAsm state (Error err) =
    assemble state $ putStrLn $ "error " ++ err
mockRunAsm state F2d = assemble state $ putStrLn "f2d"
mockRunAsm state Faload = assemble state $ putStrLn "faload"
mockRunAsm state Fastore = assemble state $ putStrLn "fastore"
mockRunAsm state (Fconst n) =
    assemble state $ putStrLn $ "fconst " ++ show n
mockRunAsm state (Field finsType cname fname desc) = assemble state $ do
  let finsTypeNum = fieldInsTypeNum finsType
  putStrLn $ unwords [
    "field",
    show finsTypeNum,
    cname,
    fname,
    desc]

mockRunAsm state FieldEnd = assemble state $ putStrLn "fieldEnd"

mockRunAsm state (Fload n) =
    assemble state $ putStrLn $ "fload " ++ show n

mockRunAsm state (Frame frameType nLocal localSigs nStack stackSigs) = assemble state $ do
  let ftypeNum = frameTypeNum frameType
  putStrLn $ unwords [
    "frame",
    show ftypeNum,
    show nLocal,
    show nStack]

mockRunAsm state Freturn = assemble state $ putStrLn "freturn"
mockRunAsm state (Fstore n) =
    assemble state $ putStrLn $ "fstore " ++ show n

mockRunAsm state (Goto label) =
    assemble state $ putStrLn $ "goto " ++ label

mockRunAsm state I2b = assemble state $ putStrLn "i2b"
mockRunAsm state I2c = assemble state $ putStrLn "i2c"
mockRunAsm state I2d = assemble state $ putStrLn "i2d"
mockRunAsm state I2l = assemble state $ putStrLn "i2l"
mockRunAsm state I2s = assemble state $ putStrLn "i2s"
mockRunAsm state Iadd = assemble state $ putStrLn "iadd"
mockRunAsm state Iaload = assemble state $ putStrLn "iaload"
mockRunAsm state Iand = assemble state $ putStrLn "iand"
mockRunAsm state Iastore = assemble state $ putStrLn "iastore"
mockRunAsm state Ior = assemble state $ putStrLn "ior"
mockRunAsm state Ixor = assemble state $ putStrLn "ixor"
mockRunAsm state Icompl = assemble state $ putStrLn "icompl"
mockRunAsm state (Iconst n) = assemble state $ putStrLn $ "iconst " ++ show n
mockRunAsm state Idiv = assemble state $ putStrLn "idiv"
mockRunAsm state (Ifeq label) =
    assemble state $ putStrLn $ "ifeq " ++ label
mockRunAsm state (Ifge label) =
    assemble state $ putStrLn $ "ifge " ++ label
mockRunAsm state (Ifgt label) =
    assemble state $ putStrLn $ "ifgt " ++ label
mockRunAsm state (Ificmpeq label) =
    assemble state $ putStrLn $ "ificmpeq " ++ label
mockRunAsm state (Ificmpge label) =
    assemble state $ putStrLn $ "ificmpge " ++ label
mockRunAsm state (Ificmpgt label) =
    assemble state $ putStrLn $ "ificmpgt " ++ label
mockRunAsm state (Ificmple label) =
    assemble state $ putStrLn $ "ificmple " ++ label
mockRunAsm state (Ificmplt label) =
    assemble state $ putStrLn $ "ificmplt " ++ label
mockRunAsm state (Ificmpne label) =
    assemble state $ putStrLn $ "ificmpne " ++ label
mockRunAsm state (Ifle label) =
    assemble state $ putStrLn $ "ifle " ++ label
mockRunAsm state (Iflt label) =
    assemble state $ putStrLn $ "iflt " ++ label
mockRunAsm state (Ifne label) =
    assemble state $ putStrLn $ "ifne " ++ label
mockRunAsm state (Ifnonnull label) =
    assemble state $ putStrLn $ "ifnonnull " ++ label
mockRunAsm state (Ifnull label) =
    assemble state $ putStrLn $ "ifnull " ++ label
mockRunAsm state (Iload n) =
    assemble state $ putStrLn $ "iload " ++ show n
mockRunAsm state Imul = assemble state $ putStrLn "imul"
mockRunAsm state Ineg = assemble state $ putStrLn "ineg"
mockRunAsm state (InstanceOf className) =
    assemble state $ putStrLn $ "instanceOf " ++ className
mockRunAsm state (InvokeMethod invocType cname mname desc isIntf) = assemble state $ do
  putStrLn $ unwords [
    "invokeMethod",
    show invocType,
    cname,
    mname,
    desc,
    show isIntf]

mockRunAsm state (InvokeDynamic mname desc handle bsmArgs) = assemble state $ do
  jbsmArgs <- sequence $ toJbsmArg <$> bsmArgs
  jhandle <- toJHandle handle
  putStrLn $ unwords [
    "invokeDynamic",
    mname,
    desc,
    !(objectToString $ the Object $ believe_me jhandle),
    !(objectToString $ the Object $ believe_me jbsmArgs)]

mockRunAsm state Irem = assemble state $ putStrLn "irem"
mockRunAsm state Ireturn = assemble state $ putStrLn "ireturn"
mockRunAsm state Ishl = assemble state $ putStrLn "ishl"
mockRunAsm state Ishr = assemble state $ putStrLn "ishr"
mockRunAsm state (Istore n) = assemble state $
    putStrLn $ "istore " ++ show n
mockRunAsm state Isub = assemble state $ putStrLn "isub"
mockRunAsm state Iushr = assemble state $ putStrLn "iushr"
mockRunAsm state L2i = assemble state $ putStrLn "l2i"
mockRunAsm state (LabelStart label) = assemble state $ putStrLn (label ++ ":")
mockRunAsm state Ladd = assemble state $ putStrLn "ladd"
mockRunAsm state Land = assemble state $ putStrLn "land"
mockRunAsm state Laload = assemble state $ putStrLn "laload"
mockRunAsm state Lastore = assemble state $ putStrLn "lastore"
mockRunAsm state Lor = assemble state $ putStrLn "lor"
mockRunAsm state Lxor = assemble state $ putStrLn "lxor"
mockRunAsm state Lcompl = assemble state $ putStrLn "lcompl"

mockRunAsm state (Ldc (TypeConst ty)) =
    assemble state $ putStrLn $ "ldcType " ++ ty
mockRunAsm state (Ldc constant) = assemble state $ do
    putStrLn ("ldc " ++ !(objectToString (constantToObject constant)))

mockRunAsm state Ldiv = assemble state $ putStrLn "ldiv"

mockRunAsm state (LineNumber lineNumber label) = assemble state $
    putStrLn $ unwords [
        "lineNumber",
        show lineNumber,
        label]

mockRunAsm state (Lload n) = assemble state $
    putStrLn $ "lload " ++ show n
mockRunAsm state Lmul = assemble state $ putStrLn "lmul"
mockRunAsm state Lneg = assemble state $ putStrLn "lneg"
mockRunAsm state (LookupSwitch defaultLabel labels cases) = assemble state $ do
  let jcases = integerValueOf <$> cases
  putStrLn $ unwords [
    "lookupSwitch",
    defaultLabel,
    !(objectToString (the Object $ believe_me labels)),
    !(objectToString (the Object $ believe_me jcases))]

mockRunAsm state (LocalVariable name descriptor signature startLabel endLabel index) = assemble state $
    putStrLn $ unwords [
        "localVariable",
        name,
        descriptor,
        (fromMaybe "null" signature),
        startLabel,
        endLabel,
        show index]

mockRunAsm state Lrem = assemble state $ putStrLn "lrem"
mockRunAsm state Lreturn = assemble state $ putStrLn "lreturn"
mockRunAsm state Lshl = assemble state $ putStrLn "lshl"
mockRunAsm state Lshr = assemble state $ putStrLn "lshr"
mockRunAsm state (Lstore n) = assemble state $
    putStrLn $ "lstore " ++ show n
mockRunAsm state Lsub = assemble state $ putStrLn "lsub"
mockRunAsm state Lushr = assemble state $ putStrLn "lushr"
mockRunAsm state (MaxStackAndLocal stack local) = assemble state $
    putStrLn $ "maxStackAndLocal " ++ show stack ++ " " ++ show local
mockRunAsm state MethodCodeStart = assemble state $
    putStrLn "methodCodeStart"
mockRunAsm state MethodCodeEnd = assemble state $ do
    putStrLn "methodCodeEnd"
    putStrLn $ "***********************************"
mockRunAsm state (Multianewarray desc dims) = assemble state $
    putStrLn $ unwords ["multiANewArray", desc, show dims]
mockRunAsm state (New cname) = assemble state $
    putStrLn $ "asmNew " ++ cname
mockRunAsm state Pop = assemble state $ putStrLn "pop"
mockRunAsm state Pop2 = assemble state $ putStrLn "pop2"
mockRunAsm state Return = assemble state $ putStrLn "voidReturn"
mockRunAsm state Saload = assemble state $ putStrLn "saload"
mockRunAsm state Sastore = assemble state $ putStrLn "sastore"
mockRunAsm state (SourceInfo sourceFileName)
  = assemble state $ putStrLn $ "sourceInfo " ++ sourceFileName
mockRunAsm state (LiftIo action) = assemble state action

mockRunAsm state (Throw fc message) = pure (believe_me $ crash $ show fc ++ ": " ++ message, state)
mockRunAsm state GetState = pure (state, state)
mockRunAsm state (SetState newState) = pure ((), newState)

mockRunAsm st (Pure value) = pure (value, st)
mockRunAsm st (Bind action f) = do
  (result, nextSt) <- mockRunAsm st action
  mockRunAsm nextSt $ f result
mockRunAsm state action = pure (believe_me $ crash "Unsupported action", state)