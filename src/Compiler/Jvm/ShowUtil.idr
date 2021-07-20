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
    showNamedCExp n (NmCon fc x tag xs) = "new " ++ show x ++ "(" ++ show tag ++
        ", " ++ (showSep "," $ showNamedCExp n <$> xs) ++ ")"
    showNamedCExp n (NmOp fc op xs) = show op ++ "(" ++
        (showSep "," $ showNamedCExp n <$> toList xs) ++ ")"
    showNamedCExp n (NmExtPrim fc p xs) = "extprim$" ++ show p ++ "(" ++
        (showSep "," $ showNamedCExp n <$> xs) ++ ")"
    showNamedCExp n (NmForce fc x) = "force" ++ "(" ++ showNamedCExp n x ++ ")"
    showNamedCExp n (NmDelay fc x) = "delay" ++ "(" ++ showNamedCExp n x ++ ")"
    showNamedCExp n (NmConCase fc sc xs def) = "\n" ++
        indent n ("constructorswitch" ++ "(" ++ showNamedCExp n sc ++ ") \n") ++
            showSep "\n" (showNamedConAlt (n + 1) <$> xs) ++
                maybe "" (\defExp => "\n" ++ indent (n + 1) "default:\n" ++
                    indent (n + 1) (showNamedCExp (n + 1) defExp)) def
    showNamedCExp n (NmConstCase fc sc xs def) = "\n" ++
        indent n ("constantswitch" ++"(" ++ show sc ++ ")\n") ++
            showSep "\n" (showNamedConstAlt (n + 1) <$> xs) ++
                maybe "" (\defExp => "\n" ++ indent (n + 1) "default:\n" ++
                    indent (n + 1) (showNamedCExp (n + 2) defExp)) def
    showNamedCExp n (NmPrimVal fc (BI value)) = "prim$BI$" ++ show value
    showNamedCExp n (NmPrimVal fc x) = "prim$" ++ show x
    showNamedCExp n (NmErased fc) = "erased"
    showNamedCExp n (NmCrash fc x) = "crash " ++ show x

    export
    showNamedConAlt : Nat -> NamedConAlt -> String
    showNamedConAlt n (MkNConAlt x tag args exp) = indent n ("case " ++ show x ++ "(" ++ show tag ++ "): \n") ++
        indent (n + 1) ("bind " ++ show args ++ ";\n") ++ (indent (n + 1) $ showNamedCExp (n + 1) exp)

    export
    showNamedConstAlt : Nat -> NamedConstAlt -> String
    showNamedConstAlt n (MkNConstAlt x exp) = indent n ("case " ++ show x ++ ":\n") ++
        indent (n + 1) (showNamedCExp (n + 1) exp)



