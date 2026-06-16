||| Pure rendering of JVM FFI binding modules from reflected class metadata.
|||
||| The compiler driver reflects classes off the classpath (see `ClasspathReflector`
||| + `Compiler.Jvm.Reflection`), builds the `ClassInfo` values below, and calls
||| `renderAll` to produce one Idris source module per Java package. This module is
||| deliberately dependency-light (only `Data.List`/Prelude) so the descriptor
||| renderer can be exercised on its own, independent of the rest of the compiler.
|||
||| Descriptor grammar produced here (see `src/Compiler/Jvm/Foreign.idr`):
|||   jvm:<spec>(<type> <type> ... <ret>),<owner>
||| where <spec> is the bare name (static), ".name" (instance), "<init>" (ctor),
||| "#name"/"#=name" (field get/set); instance members list the receiver as the
||| first paren type and the return type last; interface receivers/args carry an
||| "i:" prefix.
module Compiler.Jvm.FfiGen

import Data.List

%hide Prelude.print

--------------------------------------------------------------------------------
-- Metadata ADTs (populated by the reflector / FFI bridge)
--------------------------------------------------------------------------------

public export
data PrimTy = PBool | PByte | PChar | PShort | PInt | PLong | PFloat | PDouble | PVoid

||| A JVM type as it appears in a member signature.
||| `JRef isInterface internalName` — `isInterface` decides the `i:` prefix.
public export
data JType
  = JPrim PrimTy
  | JRef Bool String
  | JArray JType

public export
data MemberKind
  = Static
  | Instance
  | Ctor
  | FieldGet Bool   -- isStatic
  | FieldSet Bool   -- isStatic

public export
record MemberInfo where
  constructor MkMember
  jname   : String        -- JVM member name: "add", "size", "<init>", "JAVA_VERSION"
  kind    : MemberKind
  params  : List JType     -- erased param types from the descriptor (drives the jvm: string)
  ret     : JType          -- erased return type
  throws  : Bool
  genSig  : String         -- raw JVM generic method signature ("" when non-generic)

||| A referenced Java functional interface (single abstract method, e.g. `Comparator`),
||| detected by the reflector. Drives the `jlambda`-based binding so an Idris function can
||| be passed where the interface is expected, instead of constructing the interface value.
public export
record FuncIface where
  constructor MkFuncIface
  fiBinary   : String        -- interface internal name, e.g. "java/util/Comparator"
  fiSam      : String        -- single-abstract-method name, e.g. "compare"
  fiParams   : List JType    -- SAM erased param types
  fiRet      : JType         -- SAM erased return type
  fiGenSig   : String        -- SAM generic signature when declared on this interface ("" otherwise)
  fiTyParams : List String   -- this interface's own formal type params (for signature threading)

public export
record ClassInfo where
  constructor MkClass
  binary     : String        -- internal name, e.g. "java/util/ArrayList"
  isIface    : Bool
  members    : List MemberInfo
  supers     : List String    -- transitive supertypes (internal names, excl. Object)
  superArities : List (String, Nat)  -- arity of each supertype's own generic params (for parameterised markers)
  funcIfaces : List FuncIface  -- referenced functional interfaces (for jlambda bindings)
  typeParams : List String    -- generic type-parameter names, e.g. ["K","V"] (arity)

--------------------------------------------------------------------------------
-- Small string helpers (avoid String.split: the released boot's fastPack path
-- has mis-handled CONS in the past — see project memory load_crash)
--------------------------------------------------------------------------------

joinBy : String -> List String -> String
joinBy _   []        = ""
joinBy _   [x]       = x
joinBy sep (x :: xs) = x ++ sep ++ joinBy sep xs

splitOn : Char -> String -> List String
splitOn c s = go [] [] (unpack s)
  where
    go : List Char -> List String -> List Char -> List String
    go cur acc []        = reverse (pack (reverse cur) :: acc)
    go cur acc (x :: xs) =
      if x == c then go [] (pack (reverse cur) :: acc) xs
                else go (x :: cur) acc xs

capitalize : String -> String
capitalize s = case unpack s of
  []        => s
  (c :: cs) => pack (toUpper c :: cs)

sanitizeType : String -> String
sanitizeType = pack . map (\c => if c == '$' then '_' else c) . unpack

-- "java/util/ArrayList" -> (["java","util"], "ArrayList")
splitBinary : String -> (List String, String)
splitBinary s = case reverse (splitOn '/' s) of
  []          => ([], s)
  (cls :: rp) => (reverse rp, cls)

packageOf : String -> String
packageOf s = joinBy "/" (fst (splitBinary s))

simpleType : String -> String
simpleType s = sanitizeType (snd (splitBinary s))

moduleName : String -> String
moduleName pkg = joinBy "." (map capitalize (splitOn '/' pkg))

modulePath : String -> String
modulePath pkg = joinBy "/" (map capitalize (splitOn '/' pkg))

-- Fully-qualified Idris type name (e.g. "Java.Util.List") used for *references*, so a
-- generated marker like List/Map/Maybe never collides with a Prelude name. The marker's
-- own declaration and its namespace stay bare (they live in this module).
qualName : String -> String
qualName bin = moduleName (packageOf bin) ++ "." ++ simpleType bin

countNames : Nat -> List String
countNames Z     = []
countNames (S k) = countNames k ++ ["arg" ++ show k]

indentBlock : String -> String
indentBlock s = joinBy "\n" (map ("  " ++) (splitOn '\n' s))

--------------------------------------------------------------------------------
-- Type rendering
--------------------------------------------------------------------------------

-- A JVM type that Idris models with a builtin rather than an [external] marker.
isBuiltinRef : String -> Bool
isBuiltinRef n = n == "java/lang/String"

primTok : PrimTy -> String
primTok PBool   = "boolean"
primTok PByte   = "byte"
primTok PChar   = "char"
primTok PShort  = "short"
primTok PInt    = "int"
primTok PLong   = "long"
primTok PFloat  = "float"
primTok PDouble = "double"
primTok PVoid   = "void"

-- descriptor token (inside the parentheses / as owner)
tok : JType -> String
tok (JPrim p)    = primTok p
tok (JRef i n)   = if i then "i:" ++ n else n
tok (JArray t)   = "[" ++ tok t

idrisPrim : PrimTy -> String
idrisPrim PBool   = "Bool"
idrisPrim PByte   = "Bits8"
idrisPrim PChar   = "Char"
idrisPrim PShort  = "Int16"
idrisPrim PInt    = "Int"
idrisPrim PLong   = "Bits64"
idrisPrim PFloat  = "Double"
idrisPrim PDouble = "Double"
idrisPrim PVoid   = "()"

-- Idris-level type for a member parameter / return slot.
idrisTy : JType -> String
idrisTy (JPrim p)  = idrisPrim p
idrisTy (JRef _ n) = if isBuiltinRef n then "String" else simpleType n
idrisTy (JArray _) = "Object"   -- arrays opaque for the spike

--------------------------------------------------------------------------------
-- Descriptor assembly
--------------------------------------------------------------------------------

isInstanceKind : MemberKind -> Bool
isInstanceKind Instance      = True
isInstanceKind (FieldGet b)  = not b
isInstanceKind (FieldSet b)  = not b
isInstanceKind _             = False

specStr : MemberInfo -> String
specStr m = case kind m of
  Static       => jname m
  Instance     => "." ++ jname m
  Ctor         => "<init>"
  FieldGet _   => "#" ++ jname m
  FieldSet _   => "#=" ++ jname m

ownerOf : ClassInfo -> MemberInfo -> String
ownerOf ci m = case kind m of
  Ctor   => case ret m of
              JRef _ n => n
              _        => binary ci
  -- A static method declared on an interface must be invoked via an InterfaceMethodref;
  -- the "i:" owner prefix signals that to the backend (Codegen, JvmStaticMethodCall).
  Static => if isIface ci then "i:" ++ binary ci else binary ci
  _      => binary ci

export
descriptor : ClassInfo -> MemberInfo -> String
descriptor ci m =
  let recv   = if isInstanceKind (kind m) then [tok (JRef (isIface ci) (binary ci))] else []
      inside = recv ++ map tok (params m) ++ [tok (ret m)]
  in "jvm:" ++ specStr m ++ "(" ++ joinBy " " inside ++ ")," ++ ownerOf ci m

--------------------------------------------------------------------------------
-- Member name disambiguation (only needed within a class — namespaces separate
-- classes, so cross-class clashes cannot happen)
--------------------------------------------------------------------------------

-- Idris reserved words that can legally be Java member names (e.g. List.of); a
-- generated function with such a name fails to parse, so suffix it with '_'.
idrisKeywords : List String
idrisKeywords =
  [ "of", "in", "do", "let", "case", "if", "then", "else", "where", "with"
  , "data", "record", "namespace", "rewrite", "forall", "mutual", "parameters"
  , "using", "impossible", "auto", "default", "total", "partial", "covering"
  , "interface", "implementation", "module", "import", "public", "export"
  , "private", "infix", "infixl", "infixr", "prefix" ]

sanitizeName : String -> String
sanitizeName n = if n `elem` idrisKeywords then n ++ "_" else n

-- Constructors become `new` (no-arg) / `newN` (N-arg) so the zero-arg ctor reliably
-- gets the clean `new` name regardless of declaration order; collisions among
-- same-arity ctors fall through to `uniquify`.
idrisMemberName : MemberInfo -> String
idrisMemberName m = case kind m of
  Ctor => let n = length (params m) in if n == 0 then "new" else "new" ++ show n
  _    => sanitizeName (jname m)

uniquify : List MemberInfo -> List (String, MemberInfo)
uniquify = go []
  where
    free : List String -> String -> Nat -> String
    free seen base n =
      let cand = base ++ "_" ++ show n in
      if cand `elem` seen then free seen base (S n) else cand
    pick : List String -> String -> String
    pick seen base = if base `elem` seen then free seen base 2 else base
    go : List String -> List MemberInfo -> List (String, MemberInfo)
    go _    []          = []
    go seen (m :: rest) = let nm = pick seen (idrisMemberName m)
                          in (nm, m) :: go (nm :: seen) rest

--------------------------------------------------------------------------------
-- Source rendering
--------------------------------------------------------------------------------

indexed : List a -> List (Nat, a)
indexed = go 0
  where go : Nat -> List a -> List (Nat, a)
        go _ []        = []
        go i (x :: xs) = (i, x) :: go (S i) xs

rangeN : Nat -> List Nat
rangeN Z     = []
rangeN (S k) = rangeN k ++ [k]

lowerName : String -> String
lowerName s = case unpack s of
  []        => s
  (c :: cs) => pack (toLower c :: cs)

-- parenthesise a type only if it is a multi-word application (needs grouping as an arg)
paren : String -> String
paren s = if ' ' `elem` unpack s then "(" ++ s ++ ")" else s

applyTy : String -> List String -> String
applyTy h []   = h
applyTy h args = h ++ " " ++ joinBy " " args

--------------------------------------------------------------------------------
-- Generic signature parsing (JVMS 4.7.9.1): shared type variables / applied types
--------------------------------------------------------------------------------

data GType
  = GVar String              -- type variable  T<name>;
  | GCls String (List GType) -- class type + type arguments
  | GArr GType
  | GP PrimTy
  | GVoid

-- skip a balanced <...> group at the head (a method's formal type-parameter block)
skipAngles : List Char -> List Char
skipAngles ('<' :: cs) = go 1 cs
  where go : Nat -> List Char -> List Char
        go _     []          = []
        go 1     ('>' :: cs) = cs
        go d     ('>' :: cs) = go (minus d 1) cs
        go d     ('<' :: cs) = go (S d) cs
        go d     (_  :: cs)  = go d cs
skipAngles cs = cs

mutual
  parseG : List Char -> Maybe (GType, List Char)
  parseG ('T' :: cs) = let (nm, rest) = break (== ';') cs in
                       case rest of
                         (';' :: rest') => Just (GVar (pack nm), rest')
                         _              => Nothing
  parseG ('L' :: cs) =
    let (nm, rest) = break (\c => c == '<' || c == ';') cs in
    case rest of
      ('<' :: rest') => do (args, afterArgs) <- parseGArgs rest' []
                           -- drop any inner-class suffix up to the closing ';'
                           let rest'' = drop 1 (snd (break (== ';') afterArgs))
                           Just (GCls (pack nm) args, rest'')
      (';' :: rest') => Just (GCls (pack nm) [], rest')
      _              => Nothing
  parseG ('[' :: cs) = case parseG cs of
                         Just (t, rest) => Just (GArr t, rest)
                         Nothing        => Nothing
  parseG ('Z' :: cs) = Just (GP PBool, cs)
  parseG ('B' :: cs) = Just (GP PByte, cs)
  parseG ('C' :: cs) = Just (GP PChar, cs)
  parseG ('S' :: cs) = Just (GP PShort, cs)
  parseG ('I' :: cs) = Just (GP PInt, cs)
  parseG ('J' :: cs) = Just (GP PLong, cs)
  parseG ('F' :: cs) = Just (GP PFloat, cs)
  parseG ('D' :: cs) = Just (GP PDouble, cs)
  parseG ('V' :: cs) = Just (GVoid, cs)
  parseG _           = Nothing

  -- parse type arguments until the closing '>'
  parseGArgs : List Char -> List GType -> Maybe (List GType, List Char)
  parseGArgs ('>' :: cs) acc = Just (reverse acc, cs)
  parseGArgs ('*' :: cs) acc = parseGArgs cs (GCls "java/lang/Object" [] :: acc)  -- wildcard
  parseGArgs ('+' :: cs) acc = pArg cs acc
  parseGArgs ('-' :: cs) acc = pArg cs acc
  parseGArgs cs          acc = pArg cs acc

  pArg : List Char -> List GType -> Maybe (List GType, List Char)
  pArg cs acc = case parseG cs of
                  Just (t, rest) => parseGArgs rest (t :: acc)
                  Nothing        => Nothing

parseGParams : List Char -> List GType -> Maybe (List GType, List Char)
parseGParams (')' :: cs) acc = Just (reverse acc, cs)
parseGParams cs          acc = case parseG cs of
                                 Just (t, rest) => parseGParams rest (t :: acc)
                                 Nothing        => Nothing

-- (param types, return type) from a method generic signature, after its formal params
parseMethodGSig : String -> Maybe (List GType, GType)
parseMethodGSig sig = case skipAngles (unpack sig) of
  ('(' :: cs) => case parseGParams cs [] of
    Just (ps, rest) => case parseG rest of
      Just (r, _) => Just (ps, r)
      Nothing     => Nothing
    Nothing => Nothing
  _ => Nothing

methodGTypes : MemberInfo -> Maybe (List GType, GType)
methodGTypes m = if genSig m == "" then Nothing else parseMethodGSig (genSig m)

--------------------------------------------------------------------------------
-- Arity map: how many type parameters each marker type takes
--------------------------------------------------------------------------------

collectGArities : GType -> List (String, Nat)
collectGArities (GVar _)     = []
collectGArities (GCls n as)  = (n, length as) :: concatMap collectGArities as
collectGArities (GArr t)     = collectGArities t
collectGArities (GP _)       = []
collectGArities GVoid        = []

-- max arity observed for a type, from imported classes and from generic-signature usage
arityOf : List (String, Nat) -> String -> Nat
arityOf tbl n = foldl (\acc, (k, v) => if k == n then max acc v else acc) 0 tbl

arityTable : List ClassInfo -> List (String, Nat)
arityTable classes =
  let declared = map (\ci => (binary ci, length (typeParams ci))) classes
      -- supertypes are referenced but not reflected as full classes; the reflector reports
      -- their own generic arity so a marker like `AbstractList` is parameterised, not bare.
      fromSupers = concatMap superArities classes
      used = concatMap (\ci => concatMap memberUses (members ci)) classes
  in declared ++ fromSupers ++ used
  where memberUses : MemberInfo -> List (String, Nat)
        memberUses m = case methodGTypes m of
          Just (ps, r) => concatMap collectGArities (r :: ps)
          Nothing      => []

--------------------------------------------------------------------------------
-- Type rendering (parameterised)
--------------------------------------------------------------------------------

renderG : GType -> String
renderG (GVar k)     = lowerName k
renderG (GCls n as)  = if isBuiltinRef n then "String"
                       else applyTy (qualName n) (map (paren . renderG) as)
renderG (GArr _)     = "Object"
renderG (GP p)       = idrisPrim p
renderG GVoid        = "()"

-- erased JType -> Idris, padding parameterised markers with fresh type vars (seed-unique)
renderErased : (String -> Nat) -> (seed : String) -> JType -> String
renderErased ar seed (JPrim p)  = idrisPrim p
renderErased ar seed (JArray _) = "Object"
renderErased ar seed (JRef _ n) =
  if isBuiltinRef n then "String"
  else applyTy (qualName n) (map (\j => seed ++ show j) (rangeN (ar n)))

--------------------------------------------------------------------------------
-- Functional-interface (SAM) threading
--------------------------------------------------------------------------------

-- substitute type variables (by name) in a parsed generic type
substG : List (String, GType) -> GType -> GType
substG sub (GVar k)    = case lookup k sub of
                           Just t  => t
                           Nothing => GVar k
substG sub (GCls n as) = GCls n (map (substG sub) as)
substG sub (GArr t)    = GArr (substG sub t)
substG _   t           = t

-- head reference-type binary of a shape (used to spot a functional-interface argument)
gHead : GType -> Maybe String
gHead (GCls n _) = Just n
gHead _          = Nothing

gArgs : GType -> List GType
gArgs (GCls _ as) = as
gArgs _           = []

jHead : JType -> Maybe String
jHead (JRef _ n) = Just n
jHead _          = Nothing

-- type variables (Idris-cased) referenced by a parsed generic type
gVars : GType -> List String
gVars (GVar k)    = [lowerName k]
gVars (GCls _ as) = concatMap gVars as
gVars (GArr t)    = gVars t
gVars _           = []

-- The Idris function type a callback parameter accepts, e.g. "e -> e -> Int" for
-- `Comparator<e>`. When the SAM's own generic signature is known we thread the call-site
-- type arguments through it; otherwise we fall back to the erased (Object-typed) form.
samFnType : (String -> Nat) -> FuncIface -> (callArgs : List GType) -> String
samFnType ar fi callArgs =
  case (callArgs, parseMethodGSig (fiGenSig fi)) of
    (_ :: _, Just (ps, r)) =>
      let sub = zip (fiTyParams fi) callArgs
          ps' = map (substG sub) ps
      in joinBy " -> " (map (paren . renderG) ps' ++ [paren (renderG (substG sub r))])
    _ =>
      joinBy " -> " (map (renderErased ar "s_") (fiParams fi) ++ [renderErased ar "s_" (fiRet fi)])

-- per-argument plan for the wrapper
record Spec where
  constructor MkSpec
  prT : String        -- concrete type used by the prim
  sgT : String        -- wrapper parameter type (a generic var when subtyped)
  cl  : String        -- how the wrapper passes the arg to the prim
  cs  : List String   -- Inherits constraints introduced

-- A wrapper argument is either an ordinary (possibly subtyped) value, or a functional
-- interface that accepts an Idris function bridged to the SAM via `jlambda`.
data ArgPlan
  = Plain (String, Bool)   -- (concrete type, should-subtype?)
  | Lambda String String   -- (prim's marker param type, wrapper's function type)

isLambdaPlan : ArgPlan -> Bool
isLambdaPlan (Lambda _ _) = True
isLambdaPlan _            = False

mkSpec : Nat -> ArgPlan -> Spec
mkSpec i (Plain (concrete, subtyped)) =
  let an = "arg" ++ show i in
  if subtyped
    -- ascribe the upcast to the prim's concrete type: this both discharges the in-scope
    -- `Inherits` constraint and pins the prim's type variables to the wrapper's.
    then MkSpec concrete ("a" ++ show i)
               ("(the " ++ paren concrete ++ " (subtyping " ++ an ++ "))")
               ["Inherits a" ++ show i ++ " " ++ paren concrete]
    else MkSpec concrete concrete an []
mkSpec i (Lambda primTy fnTy) =
  -- pass an Idris function where a Java functional interface is expected; `jlambda`
  -- bridges it to the SAM at the JVM level (see `prim__javaLambda`).
  MkSpec primTy (paren fnTy) ("(jlambda arg" ++ show i ++ ")") []

-- (concrete type, should-subtype?) for a generic-signature argument
gShape : GType -> (String, Bool)
gShape (GVar k)    = (lowerName k, False)        -- a type variable: already most-general
gShape (GArr _)    = ("Object", False)
gShape (GP p)      = (idrisPrim p, False)
gShape GVoid       = ("()", False)
gShape (GCls n as) = if isBuiltinRef n then ("String", False)
                     else (renderG (GCls n as), True)

-- (concrete type, should-subtype?) for an erased descriptor argument
eShape : (String -> Nat) -> (Nat, JType) -> (String, Bool)
eShape ar (i, JPrim p)  = (idrisPrim p, False)
eShape ar (i, JArray _) = ("Object", False)
eShape ar (i, JRef _ n) = if isBuiltinRef n then ("String", False)
                          else (renderErased ar ("p" ++ show i ++ "_") (JRef False n), True)

--------------------------------------------------------------------------------
-- Source rendering
--------------------------------------------------------------------------------

-- one member -> its %foreign prim + subtype-polymorphic wrapper (unindented)
renderMember : (String -> Nat) -> (String -> Maybe FuncIface) -> ClassInfo -> (String, MemberInfo) -> String
renderMember ar fi ci (nm, m) =
  let classVars = map lowerName (typeParams ci)
      recvTy    = applyTy (qualName (binary ci)) classVars     -- e.g. "Java.Util.Map k v"
      isInst    = isInstanceKind (kind m)
      -- a functional-interface argument accepts an Idris function (bridged by `jlambda`);
      -- any other reference falls back to the ordinary subtyping plan.
      gPlan : GType -> ArgPlan
      gPlan g = case maybe Nothing fi (gHead g) of
        Just info => Lambda (renderG g) (samFnType ar info (gArgs g))
        Nothing   => Plain (gShape g)
      ePlan : (Nat, JType) -> ArgPlan
      ePlan (i, t) = case maybe Nothing fi (jHead t) of
        -- only bridge via jlambda when the marker takes no type parameters; otherwise (a raw
        -- generic interface, no signature to thread) its params are undeterminable, so keep
        -- the ordinary subtyping plan and let the caller supply a built interface value.
        Just info => case jHead t of
                       Just n => if ar n == 0
                                   then Lambda (renderErased ar ("p" ++ show i ++ "_") t) (samFnType ar info [])
                                   else Plain (eShape ar (i, t))
                       Nothing => Plain (eShape ar (i, t))
        Nothing   => Plain (eShape ar (i, t))
      -- parameter plans from the generic signature when present, else the erased descriptor
      paramPlans : List ArgPlan
      paramPlans = case methodGTypes m of
        Just (ps, _) => map gPlan ps
        Nothing      => map ePlan (indexed (params m))
      -- The receiver is the class's own parameterised type (direct, not subtyped): this
      -- threads the type parameters cleanly from construction through use. Subtyping is
      -- still available on parameters and via an explicit `subtyping` upcast.
      allPlans  = (if isInst then [Plain (recvTy, False)] else []) ++ paramPlans
      specs     = map (\(i, p) => mkSpec i p) (indexed allPlans)
      retTy     = case methodGTypes m of
                    Just (_, r) => renderG r
                    Nothing     => renderErased ar "rt_" (ret m)
      cons      = "HasIO io" :: concatMap cs specs
      prim      = "prim__" ++ nm
      names     = countNames (length allPlans)
      -- `jlambda` passes the interface type as a *runtime* `Type`, so any type variable in
      -- it must be bound explicitly (and non-erased) — auto-bound implicits are inaccessible.
      methodVars = case methodGTypes m of
                     Just (ps, r) => concatMap gVars (r :: ps)
                     Nothing      => []
      tyVars    = if any isLambdaPlan allPlans then nub (classVars ++ methodVars) else []
      tyBind    = concatMap (\v => "{" ++ v ++ " : Type} -> ") tyVars
      lhsBinds  = concatMap (\v => " {" ++ v ++ "}") tyVars
      lhs       = nm ++ lhsBinds ++ (if null names then "" else " " ++ joinBy " " names)
      callArgs  = map cl specs
      rhs       = if null callArgs then prim else "(" ++ prim ++ " " ++ joinBy " " callArgs ++ ")"
  in joinBy "\n"
       [ "%foreign \"" ++ descriptor ci m ++ "\""
       , prim ++ " : " ++ joinBy " -> " (map prT specs ++ ["PrimIO " ++ paren retTy])
       , "export %inline"
       , nm ++ " : " ++ tyBind ++ "(" ++ joinBy ", " cons ++ ") => "
              ++ joinBy " -> " (map sgT specs ++ ["io " ++ paren retTy])
       , lhs ++ " = primIO " ++ rhs
       ]

renderClass : (String -> Nat) -> (String -> Maybe FuncIface) -> ClassInfo -> String
renderClass ar fi ci =
  let named = uniquify (members ci)
      body  = joinBy "\n\n" (map (\p => indentBlock (renderMember ar fi ci p)) named)
  in "namespace " ++ simpleType (binary ci) ++ "\n" ++ body

-- A marker is either an opaque parameterised external type
-- (`data Map : Type -> Type -> Type where [external]`) or, for a functional interface, a
-- `Struct`-pair synonym that `jlambda` targets:
--   `Comparator a = (Struct "java/util/Comparator compare" [], Object -> Object -> Int)`
markerDecl : (String -> Nat) -> (String -> Maybe FuncIface) -> String -> String
markerDecl ar fi bin =
  let kind = joinBy " -> " (replicate (S (ar bin)) "Type")
  in case fi bin of
       Just info =>
         let vars = map (\j => "ty" ++ show j) (rangeN (ar bin))
             lhs  = applyTy (simpleType bin) vars
             fnTy = joinBy " -> " (map (renderErased ar "s_") (fiParams info)
                                   ++ [renderErased ar "s_" (fiRet info)])
             body = "(Struct \"" ++ bin ++ " " ++ fiSam info ++ "\" [], " ++ fnTy ++ ")"
         in "public export %inline\n" ++ simpleType bin ++ " : " ++ kind ++ "\n"
              ++ lhs ++ " = " ++ body
       Nothing =>
         "public export\ndata " ++ simpleType bin ++ " : " ++ kind ++ " where [external]"

-- `Inherits (Child cv..) (Parent sv..)` instances from the reflected hierarchy, plus an
-- explicit `Inherits (Child cv..) Object`. The supertype's type vars are taken from the
-- class's (identity threading, correct for the common `Foo<E> implements Bar<E>` case).
inheritsInstances : (String -> Nat) -> ClassInfo -> List String
inheritsInstances ar ci =
  let cv       = map lowerName (typeParams ci)
      childTy  = applyTy (qualName (binary ci)) cv
      objectSup = if binary ci == "java/lang/Object" then [] else ["java/lang/Object"]
      inst : String -> String
      inst s = let need    = ar s
                   svPool  = cv ++ map (\j => "x" ++ show j) (rangeN need)
                   superTy = applyTy (qualName s) (take need svPool)
               in "public export\nInherits " ++ paren childTy ++ " " ++ paren superTy ++ " where"
  in map inst (supers ci ++ objectSup)

-- The subtyping + lambda-bridging infrastructure, emitted once into Java.Lang (alongside
-- the Object marker). `prim__javaLambda` is matched by the backend on its unqualified name
-- (see ExtPrim.idr), so this self-contained copy drives `jlambda` for the generated world.
infraLines : List String
infraLines =
  [ "public export\ninterface Inherits child parent where\n  constructor MkInherits\n  export %inline\n  subtyping : child -> parent\n  subtyping = believe_me"
  , "public export\nInherits a a where"
  , "public export\nInherits String Object where"
  , "export\n%extern prim__javaLambda : (lambdaTy : Type) -> (intfTy : Type) -> (f : lambdaTy) -> intfTy"
  , "public export %inline\njlambda : {fTy : Type} -> (f : fTy) -> {intfTy : Type} -> intfTy\njlambda {fTy} f {intfTy} = prim__javaLambda fTy intfTy f"
  ]

-- reference-type binaries used by a class's signatures (excludes builtins). Includes
-- types that appear ONLY in generic signatures (e.g. Map$Entry inside Set<Map.Entry<K,V>>),
-- which the erased descriptor never mentions, so they still get a marker generated.
refsOf : ClassInfo -> List String
refsOf ci = concatMap memberRefs (members ci)
  where
    typeRef : JType -> List String
    typeRef (JRef _ n) = if isBuiltinRef n then [] else [n]
    typeRef (JArray t) = typeRef t
    typeRef (JPrim _)  = []
    gRef : GType -> List String
    gRef (GCls n as) = (if isBuiltinRef n then [] else [n]) ++ concatMap gRef as
    gRef (GArr t)    = gRef t
    gRef _           = []
    memberRefs : MemberInfo -> List String
    memberRefs m = concatMap typeRef (params m) ++ typeRef (ret m)
                ++ (case methodGTypes m of
                      Just (ps, r) => concatMap gRef (r :: ps)
                      Nothing      => [])

-- render one module (package) -> (relative .idr path, contents)
renderModule : (String -> Nat) -> (String -> Maybe FuncIface) -> List ClassInfo -> List String -> List String -> String -> (String, String)
renderModule ar fi classes imported allBins pkg =
  let binsHere     = filter (\b => packageOf b == pkg) allBins
      boundClasses = filter (\ci => (binary ci `elem` binsHere) && (binary ci `elem` imported)) classes
      depPkgs     = map packageOf (concatMap (\ci => refsOf ci ++ supers ci) boundClasses)
      importPkgs  = nub (filter (/= pkg) ("java/lang" :: depPkgs))
      -- functional-interface markers are `Struct`-pair synonyms, so pull in System.FFI
      needsStruct = not (null (mapMaybe fi binsHere))
      importLines = map (\p => "import " ++ moduleName p) importPkgs
                 ++ (if needsStruct then ["import System.FFI"] else [])
      markers     = map (markerDecl ar fi) binsHere
      infra       = if pkg == "java/lang" then infraLines else []
      instances   = concatMap (inheritsInstances ar) boundClasses
      nsBlocks    = map (renderClass ar fi) boundClasses
      header      = "-- @generated by `idris2 --jvm-ffi-import` from JVM classpath reflection.\n"
                 ++ "-- Do not edit by hand; regenerate instead."
      sections    = [header]
                 ++ ["module " ++ moduleName pkg]
                 ++ (if null importLines then [] else [joinBy "\n" importLines])
                 ++ markers
                 ++ infra
                 ++ instances
                 ++ nsBlocks
  in (modulePath pkg ++ ".idr", joinBy "\n\n" sections ++ "\n")

||| Render bindings for the given reflected classes, one Idris module per package.
||| Returns `(relativeFilePath, moduleSource)` pairs for the driver to write.
export
renderAll : List ClassInfo -> List (String, String)
renderAll classes =
  let imported = map binary classes
      allBins  = nub ("java/lang/Object" :: imported
                        ++ concatMap refsOf classes ++ concatMap supers classes)
      pkgs     = nub ("java/lang" :: map packageOf allBins)
      ar       = arityOf (arityTable classes)
      allFis   = concatMap funcIfaces classes
      fi       = \n => find (\f => fiBinary f == n) allFis
  in map (renderModule ar fi classes imported allBins) pkgs

--------------------------------------------------------------------------------
-- Parsing the reflector's line-protocol dump (see ClasspathReflector.java)
--------------------------------------------------------------------------------

-- one JVM type descriptor token: Z B C S I J F D V, L<name>;, [<type>
parseType : List Char -> Maybe (JType, List Char)
parseType ('Z' :: cs) = Just (JPrim PBool, cs)
parseType ('B' :: cs) = Just (JPrim PByte, cs)
parseType ('C' :: cs) = Just (JPrim PChar, cs)
parseType ('S' :: cs) = Just (JPrim PShort, cs)
parseType ('I' :: cs) = Just (JPrim PInt, cs)
parseType ('J' :: cs) = Just (JPrim PLong, cs)
parseType ('F' :: cs) = Just (JPrim PFloat, cs)
parseType ('D' :: cs) = Just (JPrim PDouble, cs)
parseType ('V' :: cs) = Just (JPrim PVoid, cs)
parseType ('L' :: cs) = let (nm, rest) = break (== ';') cs in
                        case rest of
                          (';' :: rest') => Just (JRef False (pack nm), rest')
                          _              => Nothing
parseType ('[' :: cs) = case parseType cs of
                          Just (t, rest) => Just (JArray t, rest)
                          Nothing        => Nothing
parseType _           = Nothing

parseParams : List Char -> Maybe (List JType, List Char)
parseParams (')' :: cs) = Just ([], cs)
parseParams cs          = case parseType cs of
                            Nothing        => Nothing
                            Just (t, rest) => case parseParams rest of
                                                Nothing          => Nothing
                                                Just (ts, rest') => Just (t :: ts, rest')

parseMethodDesc : String -> Maybe (List JType, JType)
parseMethodDesc s = case unpack s of
  ('(' :: cs) => case parseParams cs of
                   Nothing         => Nothing
                   Just (ps, rest) => case parseType rest of
                                        Just (r, _) => Just (ps, r)
                                        Nothing     => Nothing
  _ => Nothing

parseMemberLine : (binary : String) -> String -> Maybe MemberInfo
parseMemberLine bin line = case splitOn '|' line of
  ["M", name, desc, st, th, sig] => case parseMethodDesc desc of
    Nothing       => Nothing
    Just (ps, r)  =>
      let kindOf = if name == "<init>" then Ctor
                   else if st == "1" then Static else Instance
          -- the JVM ctor descriptor returns V; the jvm: form wants the class itself
          retOf  = if name == "<init>" then JRef False bin else r
      in Just (MkMember name kindOf ps retOf (th == "1") sig)
  ["F", name, desc, st] => case parseType (unpack desc) of
    Just (t, _) => Just (MkMember name (FieldGet (st == "1")) [] t False "")
    Nothing     => Nothing
  _ => Nothing

-- small decimal parser (arities are tiny; avoid Cast String Nat for boot-safety)
parseNat : String -> Nat
parseNat s = foldl step 0 (unpack s)
  where digit : Char -> Nat
        digit c = case c of
                    '0' => 0; '1' => 1; '2' => 2; '3' => 3; '4' => 4
                    '5' => 5; '6' => 6; '7' => 7; '8' => 8; '9' => 9; _ => 0
        step : Nat -> Char -> Nat
        step acc c = acc * 10 + digit c

-- "X|<name>" (legacy) or "X|<name>|<arity>" — arity is the supertype's own generic-param count
parseSuperLine : String -> Maybe (String, Nat)
parseSuperLine line = case splitOn '|' line of
  ["X", n]     => Just (n, 0)
  ["X", n, ar] => Just (n, parseNat ar)
  _            => Nothing

-- "S|<binary>|<sam>|<samDescriptor>|<samGenSig>|<ifaceFormalParams>" — a referenced
-- functional interface and its single abstract method (for the jlambda binding).
parseSamLine : String -> Maybe FuncIface
parseSamLine line = case splitOn '|' line of
  ["S", bin, sam, desc, gsig, tps] => case parseMethodDesc desc of
    Just (ps, r) => Just (MkFuncIface bin sam ps r gsig (if tps == "" then [] else splitOn ',' tps))
    Nothing      => Nothing
  _ => Nothing

||| Parse one class's reflector dump into a `ClassInfo`, or `Left` with the message
||| (including the reflector's own `ERR|...` failures).
export
parseDump : String -> Either String ClassInfo
parseDump dump = case filter (/= "") (splitOn '\n' dump) of
  []           => Left "empty reflector output"
  (hdr :: rest) => case splitOn '|' hdr of
    ("ERR" :: msg)       => Left (joinBy "|" msg)
    ["C", bin, flg, tps] => let superPairs = mapMaybe parseSuperLine rest in
                            Right (MkClass bin (flg == "1")
                                    (mapMaybe (parseMemberLine bin) rest)
                                    (map fst superPairs)
                                    superPairs
                                    (mapMaybe parseSamLine rest)
                                    (if tps == "" then [] else splitOn ',' tps))
    _                    => Left ("unexpected reflector header: " ++ hdr)
