{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for RouteEnds2
--
module RouteEnds2 where

import Data.Loc
import Data.Set as S
import Data.Map as M

import Test.Tasty.HUnit
import Test.Tasty.TH
import Test.Tasty

import Packed.FirstOrder.Common
import Packed.FirstOrder.L2.Syntax as L2
import Packed.FirstOrder.L2.Examples
import Packed.FirstOrder.Passes.RouteEnds2
import qualified Packed.FirstOrder.L1.Syntax as L1

test1 :: L Exp2
test1 = l$ Ext $ LetRegionE (VarR "r") $ l$ Ext $ LetLocE "ltest" (StartOfLE (VarR "r")) $
        l$ Ext $ LetLocE "ltest1" (AfterConstantLE 1 "ltest") $
        l$ LetE ("x", [], PackedTy "Tree" "ltest1", l$ DataConE "ltest1" "Leaf" [l$ LitE 1]) $
        l$ Ext $ LetLocE "ltest2" (AfterVariableLE "x" "ltest1") $
        l$ LetE ("y", [], PackedTy "Tree" "ltest2", l$ DataConE "ltest2" "Leaf" [l$ LitE 2]) $
        l$ LetE ("z", [], PackedTy "Tree" "ltest", l$ DataConE "ltest" "Node" [l$ VarE "x", l$ VarE "y"]) $
        l$ Ext $ LetRegionE (VarR "o") $ l$ Ext $ LetLocE "lo" (StartOfLE (VarR "o")) $
        l$ AppE "add1" ["l","lo"] (l$ VarE "z")

expectedTest1 :: Prog
expectedTest1 = Prog {ddefs = M.fromList [(Var "Tree",DDef {tyName = Var "Tree", dataCons = [("Leaf",[(False,IntTy)]),("Node",[(False,PackedTy "Tree" (Var "l")),(False,PackedTy "Tree" (Var "l"))])]})], fundefs = M.fromList [(Var "add1",L2.FunDef {funname = Var "add1", funty = ArrowTy {locVars = [LRM (Var "lin2") (VarR (Var "r3")) Input,LRM (Var "lout4") (VarR (Var "r3")) Output], arrIn = PackedTy "Tree" (Var "lin2"), arrEffs = S.fromList [Traverse (Var "lin2")], arrOut = PackedTy "Tree" (Var "lout4"), locRets = [EndOf (LRM (Var "lin2") (VarR (Var "r3")) Input)]}, funarg = Var "tr1", funbod = l$ CaseE (l$ VarE (Var "tr1")) [("Leaf",[(Var "n5",Var "l6")],l$ Ext (LetLocE (Var "jump1") (AfterConstantLE 1 (Var "l6")) $ l$ LetE (Var "v7",[],IntTy,l$ PrimAppE L1.AddP [l$ VarE (Var "n5"),l$ LitE 1]) $ l$ LetE (Var "lf8",[],PackedTy "Tree" (Var "lout4"),l$ DataConE (Var "lout4") "Leaf" [l$ VarE (Var "v7")]) $ l$ Ext (RetE [Var "jump1"] (Var "lf8")))),("Node",[(Var "x9",Var "l10"),(Var "y11",Var "l12")],l$ Ext (LetLocE (Var "l13") (AfterConstantLE 1 (Var "lout4")) $ l$ LetE (Var "x14",[Var "endof2"],PackedTy "Tree" (Var "l13"),l$ AppE (Var "add1") [Var "l10",Var "l13"] $ l$ VarE (Var "x9"))$ l$  Ext (LetLocE (Var "l12") (FromEndLE (Var "endof2")) $ l$ Ext (LetLocE (Var "l15") (AfterVariableLE (Var "x14") (Var "l13")) $ l$ LetE (Var "y16",[Var "endof3"],PackedTy "Tree" (Var "l15"), l$ AppE (Var "add1") [Var "l12",Var "l15"] $ l$ VarE (Var "y11")) $ l$ LetE (Var "z17",[],PackedTy "Tree" (Var "lout4"), l$ DataConE (Var "lout4") "Node" [l$ VarE (Var "x14"), l$ VarE (Var "y16")])$ l$ Ext (RetE [Var "endof3"] (Var "z17"))))))]})], mainExp = Just (l$ Ext (LetRegionE (VarR (Var "r")) $ l$ Ext (LetLocE (Var "ltest") (StartOfLE (VarR (Var "r"))) $ l$ Ext (LetLocE (Var "ltest1") (AfterConstantLE 1 (Var "ltest")) $ l$ LetE (Var "x",[],PackedTy "Tree" (Var "ltest1"),l$ DataConE (Var "ltest1") "Leaf" [l$ LitE 1]) $ l$ Ext (LetLocE (Var "ltest2") (AfterVariableLE (Var "x") (Var "ltest1")) $ l$ LetE (Var "y",[],PackedTy "Tree" (Var "ltest2"),l$ DataConE (Var "ltest2") "Leaf" [l$ LitE 2]) $ l$ LetE (Var "z",[],PackedTy "Tree" (Var "ltest"),l$ DataConE (Var "ltest") "Node" [l$ VarE (Var "x"),l$ VarE (Var "y")]) $ l$ Ext (LetRegionE (VarR (Var "o"))$ l$ Ext (LetLocE (Var "lo") (StartOfLE (VarR (Var "o"))) $ l$ LetE (Var "tailapp4",[Var "endof5"],PackedTy "Tree" (Var "lout4"),l$ AppE (Var "add1") [Var "l",Var "lo"]$ l$ VarE (Var "z"))$ l$  Ext (RetE [] (Var "tailapp4")))))))),IntTy)}

case_add1_test1 :: Assertion
case_add1_test1 = expectedTest1 @=? actualTest1
  where
    ddfs  = ddtree
    funs  = (M.fromList [(toVar "add1",add1TraversedFun)])
    prg   = Prog ddfs funs (Just (test1,IntTy))
    actualTest1   = fst $ runSyM 1 $ routeEnds prg

routeEnds2Tests :: TestTree
routeEnds2Tests = $(testGroupGenerator)


-- FunDef {funname = Var "add1",
-- funty = ArrowTy {locVars = [LRM (Var "lin") (VarR (Var "r1")) Input,LRM (Var "lout") (VarR (Var "r1")) Output],
--                  arrIn = PackedTy "tree" (Var "lin"),
--                  arrEffs = fromList [Traverse (Var "lin")],
--                  arrOut = PackedTy "tree" (Var "lout"),
--                  locRets = [EndOf (LRM (Var "lin") (VarR (Var "r1")) Input)]},
-- funarg = Var "tr",
-- funbod = CaseE (VarE (Var "tr"))
--          [("Leaf",[(Var "n",Var "l0")],
--           Ext (LetLocE (Var "jump1") (AfterConstantLE 1 (Var "l0") (Var "jump1"))
--           (LetE (Var "v",[],IntTy,PrimAppE AddP [VarE (Var "n"),LitE 1]) (Ext (RetE [Var "jump1"] (Var "v")))))),
--           ("Node",[(Var "x",Var "lx"),(Var "y",Var "ly")],
--           Ext (LetLocE (Var "lx1") (AfterConstantLE 1 (Var "lout") (Var "lx1"))
--           (LetE (Var "x1",[Var "endof2"],PackedTy "Tree" (Var "lx1"),AppE (Var "add1") [Var "lx",Var "lx1"] (VarE (Var "x")))
--           (Ext (LetLocE (Var "ly") (FromEndLE (Var "endof2")) (Ext (LetLocE (Var "ly1") (AfterVariableLE (Var "x1") (Var "lx1") (Var "ly1"))
--           (LetE (Var "y1",[Var "endof3"],PackedTy "Tree" (Var "ly1"),AppE (Var "add1") [Var "ly",Var "ly1"] (VarE (Var "y")))
--           (LetE (Var "z",[],PackedTy "Tree" (Var "lout"),DataConE (Var "lout") "Node" [VarE (Var "x1"),VarE (Var "y1")])
--           (ext (RetE [Var "endof3"] (Var "z"))))))))))))]}