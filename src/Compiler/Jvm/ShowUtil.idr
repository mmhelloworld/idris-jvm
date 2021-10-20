module Compiler.Jvm.ShowUtil

import Core.CompileExpr
import Core.Context
import Core.Name
import Core.TT
import Data.Vect
import Data.List

quoted : String -> String
quoted str = "\"" ++ str ++ "\""

showObj : List (String, String) -> String
showObj props = "{" ++ concat (intersperse ", " $ go <$> props) ++ "}" where
    go : (String, String) -> String
    go (k, v) = "\"" ++ k ++ "\" : " ++ v

export
showType : String -> List (String, String) -> String
showType typeName properties = showObj (("__name", quoted typeName) :: properties)

indent : Nat -> String -> String
indent n s = concat (List.replicate (n * 4) " ") ++ s

showConstant : Constant -> String
showConstant (I value)   = "prim$I$" ++ show value
showConstant (BI value)  = "prim$BI$" ++ show value
showConstant (Ch value)  = "prim$Ch$" ++ show value
showConstant (Str value) = "prim$Str$" ++ show value
showConstant (I8 value)  = "prim$I8$" ++ show value
showConstant (I16 value) = "prim$I16$" ++ show value
showConstant (I32 value) = "prim$I32$" ++ show value
showConstant (I64 value) = "prim$I64$" ++ show value
showConstant (B8 value)  = "prim$B8$" ++ show value
showConstant (B16 value) = "prim$B16$" ++ show value
showConstant (B32 value) = "prim$B32$" ++ show value
showConstant (B64 value) = "prim$B64$" ++ show value
showConstant (Db value)  = "prim$Db$" ++ show value
showConstant value       = "prim$" ++ show value

mutual
    export
    showNamedCExp : Nat -> NamedCExp -> String
    showNamedCExp n (NmLocal fc x) = show x
    showNamedCExp n (NmRef fc x) = show x
    showNamedCExp n (NmLam fc x y) = "\\" ++ show x ++ " -> " ++ showNamedCExp n y
    showNamedCExp n (NmLet fc x y z) = "\n" ++ indent n ("let " ++ show x ++ " = \n") ++
        showNamedCExp (n + 1) y ++ "\n" ++ indent n "in\n" ++ indent n (showNamedCExp (n + 1) z)
    showNamedCExp n (NmApp fc (NmRef _ name) xs) = show name ++ "(" ++
        (showSep ", " $ showNamedCExp n <$> xs) ++ ")"
    showNamedCExp n (NmApp fc x xs) = showNamedCExp n x ++ "<$>(" ++
        (showSep ", " $ showNamedCExp n <$> xs) ++ ")"
    showNamedCExp n (NmCon fc x _ tag xs) = "new " ++ show x ++ "(" ++ show tag ++
        ", " ++ (showSep "," $ showNamedCExp n <$> xs) ++ ")"
    showNamedCExp n (NmOp fc op xs) = show op ++ "(" ++
        (showSep "," $ showNamedCExp n <$> toList xs) ++ ")"
    showNamedCExp n (NmExtPrim fc p xs) = "extprim$" ++ show p ++ "(" ++
        (showSep "," $ showNamedCExp n <$> xs) ++ ")"
    showNamedCExp n (NmForce _ _ x) = "force" ++ "(" ++ showNamedCExp n x ++ ")"
    showNamedCExp n (NmDelay _ _ x) = "delay" ++ "(" ++ showNamedCExp n x ++ ")"
    showNamedCExp n (NmConCase fc sc xs def) = "\n" ++
        indent n ("constructorswitch" ++ "(" ++ showNamedCExp n sc ++ ")\n") ++
            showSep "\n" (showNamedConAlt (n + 1) <$> xs) ++
                maybe "" (\defExp => "\n" ++ indent (n + 1) "default:\n" ++
                    indent (n + 1) (showNamedCExp (n + 1) defExp)) def
    showNamedCExp n (NmConstCase fc sc xs def) = "\n" ++
        indent n ("constantswitch" ++"(" ++ showNamedCExp n sc ++ ")\n") ++
            showSep "\n" (showNamedConstAlt (n + 1) <$> xs) ++
                maybe "" (\defExp => "\n" ++ indent (n + 1) "default:\n" ++
                    indent (n + 1) (showNamedCExp (n + 2) defExp)) def
    showNamedCExp n (NmPrimVal fc x) = showConstant x
    showNamedCExp n (NmErased fc) = "erased"
    showNamedCExp n (NmCrash fc x) = "crash " ++ show x

    export
    showNamedConAlt : Nat -> NamedConAlt -> String
    showNamedConAlt n (MkNConAlt x conInfo tag args exp) = indent n ("case " ++ show x ++ show conInfo ++
      "(" ++ show tag ++ "): \n") ++ indent (n + 1) ("bind " ++ show args ++ ";\n") ++
      (indent (n + 1) $ showNamedCExp (n + 1) exp)

    export
    showNamedConstAlt : Nat -> NamedConstAlt -> String
    showNamedConstAlt n (MkNConstAlt x exp) = indent n ("case " ++ showConstant x ++ ":\n") ++
        indent (n + 1) (showNamedCExp (n + 1) exp)



