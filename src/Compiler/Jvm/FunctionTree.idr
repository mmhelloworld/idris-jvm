module Compiler.Jvm.FunctionTree

import Compiler.Common
import Compiler.CompileExpr
import Compiler.Inline

import Core.Context
import Core.Name

import Compiler.Jvm.Asm
import Compiler.Jvm.Jname
import Compiler.Jvm.Tree

import Libraries.Data.SortedSet
import Data.Vect

parameters (defs: Map String NamedDef)
    mutual
        buildFunctionTreeConAlt : List (Tree Name) -> SortedSet Name -> List NamedConAlt -> (SortedSet Name, List (Tree Name))
        buildFunctionTreeConAlt acc visited [] = (visited, acc)
        buildFunctionTreeConAlt acc visited ((MkNConAlt n _ tag args sc) :: alts) =
            let (visited, newAcc) = buildFunctionTreeExp acc visited sc
            in buildFunctionTreeConAlt newAcc visited alts

        buildFunctionTreeConstAlt : List (Tree Name) -> SortedSet Name -> List NamedConstAlt -> (SortedSet Name, List (Tree Name))
        buildFunctionTreeConstAlt acc visited [] = (visited, acc)
        buildFunctionTreeConstAlt acc visited ((MkNConstAlt c exp) :: alts) =
            let (visited, newAcc) = buildFunctionTreeExp acc visited exp
            in buildFunctionTreeConstAlt newAcc visited alts

        buildFunctionTreeArgs : List (Tree Name) -> SortedSet Name -> Vect n NamedCExp -> (SortedSet Name, List (Tree Name))
        buildFunctionTreeArgs acc visited [] = (visited, acc)
        buildFunctionTreeArgs acc visited (arg :: args) =
            let (visited, newAcc) = buildFunctionTreeExp acc visited arg
            in buildFunctionTreeArgs newAcc visited args

        buildFunctionTreeCApp : List (Tree Name) -> SortedSet Name -> List NamedCExp -> (SortedSet Name, List (Tree Name))
        buildFunctionTreeCApp acc visited [] = (visited, acc)
        buildFunctionTreeCApp acc visited (arg :: args) =
            let (visited, newAcc) = buildFunctionTreeExp acc visited arg
            in buildFunctionTreeCApp newAcc visited args

        buildFunctionTreeExp : List (Tree Name) -> SortedSet Name -> NamedCExp -> (SortedSet Name, List (Tree Name))
        buildFunctionTreeExp acc visited (NmLocal fc el) = (visited, acc)
        buildFunctionTreeExp acc visited (NmLam fc x sc) = buildFunctionTreeExp acc visited sc
        buildFunctionTreeExp acc visited (NmLet fc x val sc) =
            let (visited, valAcc) = buildFunctionTreeExp acc visited val
            in buildFunctionTreeExp valAcc visited sc
        buildFunctionTreeExp acc visited (NmApp fc (NmRef _ f) args) =
            let (visited, newAcc) = buildFunctionTreeCApp acc visited args
            in
                if SortedSet.contains f visited then
                    (visited, newAcc)
                else let (visited, child) = buildFunctionTree visited f
                     in (visited, child :: newAcc)
        buildFunctionTreeExp acc visited (NmApp fc f args) =
            let (visited, newAcc) = buildFunctionTreeCApp acc visited args
            in buildFunctionTreeExp newAcc visited f
        buildFunctionTreeExp acc visited (NmCon fc name _ tag args) = buildFunctionTreeCApp acc visited args
        buildFunctionTreeExp acc visited (NmOp fc op args) = buildFunctionTreeArgs acc visited args
        buildFunctionTreeExp acc visited (NmExtPrim fc p args) = buildFunctionTreeCApp acc visited args
        buildFunctionTreeExp acc visited (NmForce fc _ t) = buildFunctionTreeExp acc visited t
        buildFunctionTreeExp acc visited (NmDelay fc _ t) = buildFunctionTreeExp acc visited t
        buildFunctionTreeExp acc visited (NmConCase fc sc alts def)
            = let (visited, accSc) = buildFunctionTreeExp acc visited sc
                  (visited, accDefc) = maybe ((visited, accSc)) (\v => buildFunctionTreeExp accSc visited v) def
              in buildFunctionTreeConAlt accDefc visited alts
        buildFunctionTreeExp acc visited (NmConstCase fc sc alts def)
            = let (visited, accDefc) = maybe ((visited, acc)) (\v => buildFunctionTreeExp acc visited v) def
                  (visited, accSc) = buildFunctionTreeExp accDefc visited sc
              in buildFunctionTreeConstAlt accSc visited alts
        buildFunctionTreeExp acc visited (NmPrimVal fc c) = (visited, acc)
        buildFunctionTreeExp acc visited (NmErased fc) = (visited, acc)
        buildFunctionTreeExp acc visited (NmCrash fc msg) = (visited, acc)
        buildFunctionTreeExp acc visited expr = (visited, acc)

        buildFunctionTreeDef : SortedSet Name -> Name -> NamedDef -> (SortedSet Name, List (Tree Name))
        buildFunctionTreeDef visited n (MkNmFun args exp) = buildFunctionTreeExp [] visited exp
        buildFunctionTreeDef visited n (MkNmError exp) = buildFunctionTreeExp [] visited exp
        buildFunctionTreeDef visited n (MkNmForeign _ _ _) = (visited, [])
        buildFunctionTreeDef visited n (MkNmCon _ _ _) = (visited, [])

        findDef : Name -> Maybe NamedDef
        findDef name =
            let nameStr = jvmSimpleName name
            in nullableToMaybe $ unsafePerformIO $ Map.get defs nameStr

        buildFunctionTree : SortedSet Name -> Name -> (SortedSet Name, Tree Name)
        buildFunctionTree visitedSoFar name =
            let visited = SortedSet.insert name visitedSoFar
            in case findDef name of
               Nothing => (visited, Node name [])
               Just d => let (visited, children) = buildFunctionTreeDef visited name d
                    in (visited, Node name children)

export
buildFunctionTreeMain : Name -> Map String NamedDef -> Tree Name
buildFunctionTreeMain mainFunctionName defs = snd $ buildFunctionTree defs SortedSet.empty mainFunctionName
