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

public export
record ClassInfo where
  constructor MkClass
  binary     : String        -- internal name, e.g. "java/util/ArrayList"
  isIface    : Bool
  members    : List MemberInfo
  supers     : List String    -- transitive supertypes (internal names, excl. Object)
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
      used = concatMap (\ci => concatMap memberUses (members ci)) classes
  in declared ++ used
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

-- per-argument plan for the wrapper
record Spec where
  constructor MkSpec
  prT : String        -- concrete type used by the prim
  sgT : String        -- wrapper parameter type (a generic var when subtyped)
  cl  : String        -- how the wrapper passes the arg to the prim
  cs  : List String   -- Inherits constraints introduced

mkSpec : Nat -> (String, Bool) -> Spec
mkSpec i (concrete, subtyped) =
  let an = "arg" ++ show i in
  if subtyped
    -- ascribe the upcast to the prim's concrete type: this both discharges the in-scope
    -- `Inherits` constraint and pins the prim's type variables to the wrapper's.
    then MkSpec concrete ("a" ++ show i)
               ("(the " ++ paren concrete ++ " (subtyping " ++ an ++ "))")
               ["Inherits a" ++ show i ++ " " ++ paren concrete]
    else MkSpec concrete concrete an []

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
renderMember : (String -> Nat) -> ClassInfo -> (String, MemberInfo) -> String
renderMember ar ci (nm, m) =
  let classVars = map lowerName (typeParams ci)
      recvTy    = applyTy (qualName (binary ci)) classVars     -- e.g. "Java.Util.Map k v"
      isInst    = isInstanceKind (kind m)
      -- parameter shapes from the generic signature when present, else the erased descriptor
      paramShapes : List (String, Bool)
      paramShapes = case methodGTypes m of
        Just (ps, _) => map gShape ps
        Nothing      => map (eShape ar) (indexed (params m))
      -- The receiver is the class's own parameterised type (direct, not subtyped): this
      -- threads the type parameters cleanly from construction through use. Subtyping is
      -- still available on parameters and via an explicit `subtyping` upcast.
      allShapes = (if isInst then [(recvTy, False)] else []) ++ paramShapes
      specs     = map (\(i, sh) => mkSpec i sh) (indexed allShapes)
      retTy     = case methodGTypes m of
                    Just (_, r) => renderG r
                    Nothing     => renderErased ar "rt_" (ret m)
      cons      = "HasIO io" :: concatMap cs specs
      prim      = "prim__" ++ nm
      names     = countNames (length allShapes)
      lhs       = nm ++ (if null names then "" else " " ++ joinBy " " names)
      callArgs  = map cl specs
      rhs       = if null callArgs then prim else "(" ++ prim ++ " " ++ joinBy " " callArgs ++ ")"
  in joinBy "\n"
       [ "%foreign \"" ++ descriptor ci m ++ "\""
       , prim ++ " : " ++ joinBy " -> " (map prT specs ++ ["PrimIO " ++ paren retTy])
       , "export %inline"
       , nm ++ " : (" ++ joinBy ", " cons ++ ") => "
              ++ joinBy " -> " (map sgT specs ++ ["io " ++ paren retTy])
       , lhs ++ " = primIO " ++ rhs
       ]

renderClass : (String -> Nat) -> ClassInfo -> String
renderClass ar ci =
  let named = uniquify (members ci)
      body  = joinBy "\n\n" (map (\p => indentBlock (renderMember ar ci p)) named)
  in "namespace " ++ simpleType (binary ci) ++ "\n" ++ body

-- parameterised marker: `data Map : Type -> Type -> Type where [external]`
markerDecl : (String -> Nat) -> String -> String
markerDecl ar bin =
  let kind = joinBy " -> " (replicate (S (ar bin)) "Type")
  in "public export\ndata " ++ simpleType bin ++ " : " ++ kind ++ " where [external]"

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

-- The subtyping infrastructure, emitted once into Java.Lang (alongside the Object marker).
infraLines : List String
infraLines =
  [ "public export\ninterface Inherits child parent where\n  constructor MkInherits\n  export %inline\n  subtyping : child -> parent\n  subtyping = believe_me"
  , "public export\nInherits a a where"
  , "public export\nInherits String Object where"
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
renderModule : (String -> Nat) -> List ClassInfo -> List String -> List String -> String -> (String, String)
renderModule ar classes imported allBins pkg =
  let binsHere     = filter (\b => packageOf b == pkg) allBins
      boundClasses = filter (\ci => (binary ci `elem` binsHere) && (binary ci `elem` imported)) classes
      depPkgs     = map packageOf (concatMap (\ci => refsOf ci ++ supers ci) boundClasses)
      importPkgs  = nub (filter (/= pkg) ("java/lang" :: depPkgs))
      importLines = map (\p => "import " ++ moduleName p) importPkgs
      markers     = map (markerDecl ar) binsHere
      infra       = if pkg == "java/lang" then infraLines else []
      instances   = concatMap (inheritsInstances ar) boundClasses
      nsBlocks    = map (renderClass ar) boundClasses
      sections    = ["module " ++ moduleName pkg]
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
  in map (renderModule ar classes imported allBins) pkgs

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

parseSuperLine : String -> Maybe String
parseSuperLine line = case splitOn '|' line of
  ["X", n] => Just n
  _        => Nothing

||| Parse one class's reflector dump into a `ClassInfo`, or `Left` with the message
||| (including the reflector's own `ERR|...` failures).
export
parseDump : String -> Either String ClassInfo
parseDump dump = case filter (/= "") (splitOn '\n' dump) of
  []           => Left "empty reflector output"
  (hdr :: rest) => case splitOn '|' hdr of
    ("ERR" :: msg)       => Left (joinBy "|" msg)
    ["C", bin, flg, tps] => Right (MkClass bin (flg == "1")
                                    (mapMaybe (parseMemberLine bin) rest)
                                    (mapMaybe parseSuperLine rest)
                                    (if tps == "" then [] else splitOn ',' tps))
    _                    => Left ("unexpected reflector header: " ++ hdr)
