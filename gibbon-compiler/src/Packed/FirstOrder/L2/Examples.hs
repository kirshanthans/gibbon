{-# LANGUAGE OverloadedStrings #-}

module Packed.FirstOrder.L2.Examples
  ( -- * Data definitions
    ddtree
    -- * Functions
  , add1Fun, add1TraversedFun, id1Fun, copyTreeFun, id2Fun, id3Fun, intAddFun
  , leftmostFun, buildLeafFun, testProdFun

    -- * Programs
  , add1Prog, id1Prog, copyTreeProg, id2Prog, copyOnId1Prog, id3Prog, intAddProg
  , leftmostProg, buildLeafProg, testProdProg, nodeProg, leafProg, testFlattenProg
  , rightmostProg, buildTreeProg, buildTreeSumProg, printTupProg, addTreesProg
  , printTupProg2, sumUpProg
  ) where

import Data.Loc
import Data.Set as S
import Data.Map as M
-- import Text.PrettyPrint.GenericPretty

import Packed.FirstOrder.Common hiding (FunDef)
import Packed.FirstOrder.L2.Syntax
import Packed.FirstOrder.L1.Syntax hiding (Prog, FunDef, ddefs, fundefs, mainExp, add1Prog)
import Packed.FirstOrder.GenericOps

ddtree :: DDefs Ty2
ddtree = fromListDD [DDef (toVar "Tree")
                      [ ("Leaf",[(False,IntTy)])
                      , ("Node",[ (False,PackedTy "Tree" "l")
                                , (False,PackedTy "Tree" "l")])
                      ]]


tTypeable :: L Exp2
tTypeable =  l$ Ext $ LetRegionE (VarR "r500") $
             l$ Ext $ LetLocE "l501" (StartOfLE (VarR "r500")) $
             l$ LetE ("v502",[], IntTy, l$ LitE 42) $
             l$ (VarE "v502")

testTypeable :: UrTy LocVar
testTypeable = gTypeExp ddtree emptyEnv2 tTypeable

--------------------------------------------------------------------------------
-- Add1

add1TraversedFun :: FunDef
add1TraversedFun = FunDef "add1" add1TraversedFunTy "tr1" add1FunBod
  where add1TraversedFunTy = add1FunTy { arrEffs = S.fromList [Traverse "lin2"] }


add1Fun :: FunDef
add1Fun = FunDef "add1" add1FunTy "tr1" add1FunBod


add1FunTy :: ArrowTy Ty2
add1FunTy = (ArrowTy
             [LRM "lin2" (VarR "r3") Input, LRM "lout4" (VarR "r3") Output]
             (PackedTy "Tree" "lin2")
             (S.empty)
             (PackedTy "Tree" "lout4")
             [])


add1FunBod :: L Exp2
add1FunBod = l$ CaseE (l$ VarE "tr1") $
  [ ("Leaf", [("n5","l6")],
      l$ LetE ("v7",[],IntTy,
               l$ PrimAppE AddP [l$ VarE "n5", l$ LitE 1]) $
      l$ LetE ("lf8",[],PackedTy "Tree" "lout4",
               l$ DataConE "lout4" "Leaf" [l$ VarE "v7"]) $
      l$ VarE "lf8")

  , ("Node", [("x9","l10"),("y11","l12")],
     l$ Ext $ LetLocE "l13" (AfterConstantLE 1 "lout4") $
     l$ LetE ("x14",[],PackedTy "Tree" "l13",
               l$ AppE "add1" ["l10","l13"] (l$ VarE "x9")) $
     l$ Ext $ LetLocE "l15" (AfterVariableLE "x14" "l13") $
     l$ LetE ("y16",[],PackedTy "Tree" "l15", l$ AppE "add1" ["l12","l15"] (l$ VarE "y11")) $
     l$ LetE ("z17",[],PackedTy "Tree" "lout4",
              l$ DataConE "lout4" "Node" [ l$ VarE "x14" , l$ VarE "y16"]) $
     l$ VarE "z17")
  ]

add1MainExp :: L Exp2
add1MainExp = l$ Ext $ LetRegionE (VarR "r99") $
              l$ Ext $ LetLocE "l100" (StartOfLE (VarR "r99")) $
              l$ Ext $ LetLocE "l101" (AfterConstantLE 1 "l100") $
              l$ LetE ("x102",[],PackedTy "Tree" "l101",
                      l$ DataConE "l101" "Leaf" [l$ LitE 1]) $
              l$ Ext $ LetLocE "l103" (AfterVariableLE "x102" "l101") $
              l$ LetE ("y104",[],PackedTy "Tree" "l103",
                      l$ DataConE "l103" "Leaf" [l$ LitE 2]) $
              l$ LetE ("z105",[],PackedTy "Tree" "l100",
                      l$ DataConE "l100" "Node" [l$ VarE "x102",
                                                 l$ VarE "y104"]) $
              l$ Ext $ LetRegionE (VarR "r106") $
              l$ Ext $ LetLocE "l107" (StartOfLE (VarR "r106")) $
              l$ LetE ("a108",[], PackedTy "Tree" "l107",
                      l$ AppE "add1" ["l100", "l107"] (l$ VarE "z105")) $
              l$ VarE "a108"


add1Prog :: Prog
add1Prog = Prog ddtree (M.fromList [("add1", add1Fun)])
           (Just (add1MainExp, PackedTy "Tree" "l107"))

--------------------------------------------------------------------------------

leafMainExp :: L Exp2
leafMainExp = l$ Ext $ LetRegionE (VarR "r150") $
              l$ Ext $ LetLocE "l151" (StartOfLE (VarR "r150")) $
              l$ LetE ("x152",[],PackedTy "Tree" "l151",
                       l$ DataConE "l151" "Leaf" [l$ LitE 1]) $
              l$ VarE "x152"

leafProg :: Prog
leafProg = Prog ddtree (M.empty) (Just (leafMainExp, PackedTy "Tree" "l151"))


--------------------------------------------------------------------------------

-- writes node
nodeMainExp :: L Exp2
nodeMainExp = l$ Ext $ LetRegionE (VarR "r155") $
               l$ Ext $ LetLocE "l156" (StartOfLE (VarR "r155")) $
               l$ Ext $ LetLocE "l157" (AfterConstantLE 1 "l156") $
               l$ LetE ("x158",[],PackedTy "Tree" "l157",
                       l$ DataConE "l157" "Leaf" [l$ LitE 1]) $
               l$ Ext $ LetLocE "l159" (AfterVariableLE "x158" "l157") $
               l$ LetE ("y160",[],PackedTy "Tree" "l159",
                       l$ DataConE "l159" "Leaf" [l$ LitE 2]) $
               l$ LetE ("z161",[],PackedTy "Tree" "l156",
                       l$ DataConE "l156" "Node" [l$ VarE "x158", l$ VarE "y160"]) $
               l$ VarE "z161"


nodeProg :: Prog
nodeProg = Prog ddtree (M.empty) (Just (nodeMainExp, PackedTy "Tree" "l156"))

--------------------------------------------------------------------------------

id1Fun :: FunDef
id1Fun = FunDef "id1" idFunTy "tr18" idFunBod
  where
    idFunBod = (l$ VarE "tr18")

    idFunTy :: ArrowTy Ty2
    idFunTy = (ArrowTy
               [LRM "lin19" (VarR "r20") Input, LRM "lout21" (VarR "r20") Output]
               (PackedTy "Tree" "lin19")
               (S.empty)
               (PackedTy "Tree" "lout21")
               [])


id1Prog :: Prog
id1Prog = Prog ddtree (M.fromList [("id1", id1Fun)]) Nothing

--------------------------------------------------------------------------------

copyTreeFun :: FunDef
copyTreeFun = FunDef "copyTree" copyFunTy "tr22" copyBod
  where
    copyFunTy = (ArrowTy
                 [LRM "lin23" (VarR "r24") Input, LRM "lout25" (VarR "r24") Output]
                 (PackedTy "Tree" "lin23")
                 S.empty
                 (PackedTy "Tree" "lout25")
                 [])

    copyBod = l$ CaseE (l$ VarE "tr22") $
                 [ ("Leaf", [("n27","lin26")],
                     l$ LetE ("n28",[],PackedTy "Tree" "lout25",
                               l$ DataConE "lout25" "Leaf" [l$ VarE "n27"]) $
                     l$ VarE "n28")

                 , ("Node", [("x29","lx30"),("y31","ly32")],
                    l$ Ext  $ LetLocE "lx33" (AfterConstantLE 1 "lout25") $
                    l$ LetE ("x34", [], PackedTy "Tree" "lx33",
                             l$ AppE "copyTree" ["lx30","lx33"] (l$ VarE "x29")) $
                    l$ Ext  $ LetLocE "ly35" (AfterVariableLE "x34" "lx33") $
                    l$ LetE ("y36", [], PackedTy "Tree" "ly35",
                            l$ AppE "copyTree" ["ly32","ly35"] (l$ VarE "y31")) $
                    l$ DataConE "lout25" "Node" [l$ VarE "x34", l$ VarE "y36"])
                 ]

copyTreeMainExp :: L Exp2
copyTreeMainExp = l$ Ext $ LetRegionE (VarR "r200") $
                  l$ Ext $ LetLocE "l201" (StartOfLE (VarR "r200")) $
                  l$ Ext $ LetLocE "l202" (AfterConstantLE 1 "l201") $
                  l$ LetE ("x203",[],PackedTy "Tree" "l202",
                          l$ DataConE "l202" "Leaf" [l$ LitE 1]) $
                  l$ Ext $ LetLocE "r204" (AfterVariableLE "x203" "l202") $
                  l$ LetE ("y205",[],PackedTy "Tree" "r204",
                           l$ DataConE "r204" "Leaf" [l$ LitE 2]) $
                  l$ LetE ("z206",[],PackedTy "Tree" "l201",
                           l$ DataConE "l201" "Node" [l$ VarE "x203", l$ VarE "y205"]) $
                  l$ Ext $ LetRegionE (VarR "r207") $
                  l$ Ext $ LetLocE "l208" (StartOfLE (VarR "r207")) $
                  l$ LetE ("a209",[], PackedTy "Tree" "l208",
                           l$ AppE "copyTree" ["l201", "l208"] (l$ VarE "z206")) $
                  l$ VarE "a209"

copyTreeProg :: Prog
copyTreeProg = Prog ddtree (M.fromList [("copyTree", copyTreeFun)]) $
               Just (copyTreeMainExp, PackedTy "Tree" "l208")

--------------------------------------------------------------------------------

id2Fun :: FunDef
id2Fun = FunDef "id2" id2Ty "tr41" id2Bod
  where
    id2Ty :: ArrowTy Ty2
    id2Ty = (ArrowTy
             [LRM "lin37" (VarR "r38") Input, LRM "lout39" (VarR "r38") Output]
             (PackedTy "Tree" "lin37")
             (S.empty)
             (PackedTy "Tree" "lout39")
             [])

    id2Bod = l$ IfE (l$ PrimAppE EqIntP [l$ LitE 20, l$ LitE 20])
             (l$ (VarE "tr41"))
             (l$ (VarE "tr41"))

id2Prog :: Prog
id2Prog = Prog ddtree (M.fromList [("id2", id2Fun)]) Nothing

--------------------------------------------------------------------------------

copyOnId1Prog :: Prog
copyOnId1Prog = Prog ddtree funs $ Just (copyOnId1MainExp, PackedTy "Tree" "l228")
  where
    funs  = (M.fromList [("copyTree" , copyTreeFun),
                         ("id1WithCopy", id1WithCopyFun)])

copyOnId1MainExp :: L Exp2
copyOnId1MainExp = l$ Ext $ LetRegionE (VarR "r220") $
                   l$ Ext $ LetLocE "l221" (StartOfLE (VarR "r220")) $
                   l$ Ext $ LetLocE "l222" (AfterConstantLE 1 "l221") $
                   l$ LetE ("l223",[],PackedTy "Tree" "l222",
                           l$ DataConE "l222" "Leaf" [l$ LitE 1]) $
                   l$ Ext $ LetLocE "l224" (AfterVariableLE "l223" "l222") $
                   l$ LetE ("l225",[],PackedTy "Tree" "l224",
                            l$ DataConE "l224" "Leaf" [l$ LitE 2]) $
                   l$ LetE ("z226",[],PackedTy "Tree" "l221",
                            l$ DataConE "l221" "Node" [l$ VarE "l223", l$ VarE "l225"]) $
                   l$ Ext $ LetRegionE (VarR "r227") $
                   l$ Ext $ LetLocE "l228" (StartOfLE (VarR "r227")) $
                   l$ LetE ("a229",[], PackedTy "Tree" "l228",
                            l$ AppE "id1WithCopy" ["l221", "l228"] (l$ VarE "z226")) $
                   l$ VarE "a229"

id1WithCopyFun :: FunDef
id1WithCopyFun = id1Fun { funbod = l$ AppE "copyTree" ["lin19","lout21"]
                                   (l$ VarE "tr18")
                        , funname = "id1WithCopy"
                        }

--------------------------------------------------------------------------------

id3Fun :: FunDef
id3Fun = FunDef "id3" id3Ty "i42" id3Bod
  where
    id3Ty :: ArrowTy Ty2
    id3Ty = (ArrowTy
             []
             (IntTy)
             (S.empty)
             (IntTy)
             [])
    id3Bod = l$ VarE "i42"

id3MainExp :: L Exp2
id3MainExp = l$ AppE "id3" [] (l$ LitE 42)

id3Prog :: Prog
id3Prog = Prog ddtree (M.fromList [("id3", id3Fun)]) $ Just (id3MainExp, IntTy)


--------------------------------------------------------------------------------

intAddFun :: FunDef
intAddFun = FunDef "intAdd" intAddTy "i109" id3Bod
  where
    intAddTy :: ArrowTy Ty2
    intAddTy = (ArrowTy
                []
                (ProdTy [IntTy, IntTy])
                (S.empty)
                (IntTy)
                [])
    id3Bod = l$ PrimAppE AddP [l$ ProjE 0 (l$ VarE "i109"), l$ ProjE 1 (l$ VarE "i109")]

intAddMainExp :: L Exp2
intAddMainExp = l$ LetE ("sum110", [], IntTy,
                         l$ AppE "intAdd" []
                         (l$ MkProdE [l$LitE 40,l$LitE 2])) $
                l$ (VarE "sum110")

intAddProg :: Prog
intAddProg = Prog M.empty (M.fromList [("intAdd", intAddFun)]) (Just (intAddMainExp, IntTy))

--------------------------------------------------------------------------------

leftmostFun :: FunDef
leftmostFun = FunDef "leftmost" leftmostTy "t111" leftmostBod
  where
    leftmostTy :: ArrowTy Ty2
    leftmostTy = (ArrowTy
                 [LRM "lin112" (VarR "r113") Input]
                 (PackedTy "Tree" "lin112")
                 (S.empty)
                 (IntTy)
                 [])

leftmostBod :: L Exp2
leftmostBod = l$ CaseE (l$ VarE "t111")
              [("Leaf", [("n114","l115")],
                l$ VarE "n114"),
               ("Node", [("x117","l118"), ("y119","l120")],
                l$ LetE ("lm121",[],IntTy, l$ AppE "leftmost" ["l118"] (l$ VarE "x117")) $
                l$ VarE "lm121")]

leftmostMainExp :: L Exp2
leftmostMainExp = l$ Ext $ LetRegionE (VarR "r122") $
                  l$ Ext $ LetLocE "l123" (StartOfLE (VarR "r122")) $
                  l$ Ext $ LetLocE "l124" (AfterConstantLE 1 "l123") $
                  l$ LetE ("x125",[],PackedTy "Tree" "l124",
                          l$ DataConE "l124" "Leaf" [l$ LitE 1]) $
                  l$ Ext $ LetLocE "l126" (AfterVariableLE "x125" "l124") $
                  l$ LetE ("y128",[],PackedTy "Tree" "l126",
                          l$ DataConE "l126" "Leaf" [l$ LitE 2]) $
                  l$ LetE ("z127",[],PackedTy "Tree" "l123",
                          l$ DataConE "l123" "Node" [l$ VarE "x125", l$ VarE "y128"]) $
                  l$ LetE ("a131",[], IntTy,
                          l$ AppE "leftmost" ["l123"] (l$ VarE "z127")) $
                  l$ VarE "a131"

leftmostProg :: Prog
leftmostProg = Prog ddtree (M.fromList [("leftmost", leftmostFun)]) (Just (leftmostMainExp, IntTy))


--------------------------------------------------------------------------------

rightmostFun :: FunDef
rightmostFun = FunDef "rightmost" rightmostTy "t242" rightmostBod
  where
    rightmostTy :: ArrowTy Ty2
    rightmostTy = (ArrowTy
                   [LRM "lin241" (VarR "r240") Input]
                   (PackedTy "Tree" "lin241")
                   (S.empty)
                   (IntTy)
                   [])

rightmostBod :: L Exp2
rightmostBod = l$ CaseE (l$ VarE "t242")
               [("Leaf", [("n246","l247")],
                 l$ VarE "n246"),
                ("Node", [("x248","l249"), ("y250","l251")],
                 l$ Ext $ LetRegionE (DynR "r252") $
                 l$ Ext $ LetLocE "l253" (StartOfLE (DynR "r252")) $
                 l$ LetE ("x254",[],PackedTy "Tree" "l253",
                        l$ AppE "copyTree" ["l249", "l253"] (l$ VarE "x248")) $
                 l$ LetE ("lm255",[],IntTy, l$ AppE "rightmost" ["l251"] (l$ VarE "y250")) $
                 l$ VarE "lm255")]

rightmostMainExp :: L Exp2
rightmostMainExp = l$ Ext $ LetRegionE (VarR "r253") $
                   l$ Ext $ LetLocE "l254" (StartOfLE (VarR "r253")) $
                   l$ Ext $ LetLocE "l255" (AfterConstantLE 1 "l254") $
                   l$ LetE ("x256",[],PackedTy "Tree" "l255",
                            l$ DataConE "l255" "Leaf" [l$ LitE 1]) $
                   l$ Ext $ LetLocE "l257" (AfterVariableLE "x256" "l255") $
                   l$ LetE ("y258",[],PackedTy "Tree" "l257",
                            l$ DataConE "l257" "Leaf" [l$ LitE 2]) $
                   l$ LetE ("z259",[],PackedTy "Tree" "l254",
                            l$ DataConE "l254" "Node" [l$ VarE "x256", l$ VarE "y258"]) $
                   l$ LetE ("a260",[], IntTy,
                            l$ AppE "rightmost" ["l254"] (l$ VarE "z259")) $
                   l$ VarE "a260"

rightmostProg :: Prog
rightmostProg = Prog ddtree (M.fromList [("rightmost", rightmostFun), ("copyTree",copyTreeFun)])
                (Just (rightmostMainExp, IntTy))


--------------------------------------------------------------------------------

buildLeafFun :: FunDef
buildLeafFun = FunDef "buildLeaf" buildLeafTy "i125" buildLeafBod
  where
    buildLeafTy :: ArrowTy Ty2
    buildLeafTy = (ArrowTy
                   [LRM "lout126" (VarR "r127") Output]
                   (IntTy)
                   (S.empty)
                   (PackedTy "Tree" "lout126")
                   [])

    buildLeafBod :: L Exp2
    buildLeafBod = l$ DataConE "lout126" "Leaf" [l$ VarE "i125"]


buildLeafMainExp :: L Exp2
buildLeafMainExp = l$ Ext $ LetRegionE (VarR "r128") $
                   l$ Ext $ LetLocE "l129" (StartOfLE (VarR "r128")) $
                   l$ AppE "buildLeaf" ["l129"] (l$ LitE 42)

buildLeafProg :: Prog
buildLeafProg = Prog ddtree (M.fromList [("buildLeaf", buildLeafFun)]) (Just (buildLeafMainExp, PackedTy "Tree" "l129"))


--------------------------------------------------------------------------------

buildTreeFun :: FunDef
buildTreeFun = FunDef "buildTree" buildTreeTy "i270" buildTreeBod
  where
    buildTreeTy :: ArrowTy Ty2
    buildTreeTy = (ArrowTy
                   [LRM "lout272" (VarR "r271") Output]
                   (IntTy)
                   (S.empty)
                   (PackedTy "Tree" "lout272")
                   [])

    buildTreeBod :: L Exp2
    buildTreeBod = l$ LetE ("b279",[], BoolTy, l$ PrimAppE EqIntP [l$ VarE "i270", l$ LitE 0]) $
                   l$ IfE (l$ VarE "b279")
                   (l$ DataConE "lout272" "Leaf" [l$ LitE 1])
                   (l$ LetE ("i273",[], IntTy, l$ PrimAppE SubP [l$ VarE "i270", l$ LitE 1]) $
                    l$ Ext $ LetLocE "l274" (AfterConstantLE 1 "lout272") $
                    l$ LetE ("x275",[],PackedTy "Tree" "l274",
                             l$ AppE "buildTree" ["l274"] (l$ VarE "i273")) $
                    l$ Ext $ LetLocE "l276" (AfterVariableLE "x275" "l274") $
                    l$ LetE ("y277",[],PackedTy "Tree" "l276",
                             l$ AppE "buildTree" ["l276"] (l$ VarE "i273")) $
                    l$ LetE ("a278",[],PackedTy "Tree" "lout272",
                             l$ DataConE "lout272" "Node" [l$ VarE "x275", l$ VarE "y277"]) $
                    l$ VarE "a278")


buildTreeMainExp :: L Exp2
buildTreeMainExp = l$ Ext $ LetRegionE (VarR "r279") $
                   l$ Ext $ LetLocE "l280" (StartOfLE (VarR "r279")) $
                   l$ AppE "buildTree" ["l280"] (l$ LitE 3)

buildTreeProg :: Prog
buildTreeProg = Prog ddtree (M.fromList [("buildTree", buildTreeFun)]) (Just (buildTreeMainExp, PackedTy "Tree" "l280"))


--------------------------------------------------------------------------------

buildTreeSumFun :: FunDef
buildTreeSumFun = FunDef "buildTreeSum" buildTreeSumTy "i302" buildTreeSumBod
  where
    buildTreeSumTy :: ArrowTy Ty2
    buildTreeSumTy = (ArrowTy
                      [LRM "lout301" (VarR "r300") Output]
                      (IntTy)
                      (S.empty)
                      (ProdTy [IntTy, PackedTy "Tree" "lout301"])
                      [])

    buildTreeSumBod :: L Exp2
    buildTreeSumBod = l$ LetE ("b303",[], BoolTy, l$ PrimAppE EqIntP [l$ VarE "i302", l$ LitE 0]) $
                      l$ IfE (l$ VarE "b303")
                      (l$ LetE ("c316",[],PackedTy "Tree" "lout301",
                                l$ DataConE "lout301" "Leaf" [l$ LitE 1]) $
                       l$ LetE ("t317",[],ProdTy [IntTy, PackedTy "Tree" "lout301"],
                               l$ MkProdE [l$ LitE 1, l$ VarE "c316"]) $
                       l$ VarE "t317")
                      (l$ LetE ("i303",[], IntTy, l$ PrimAppE SubP [l$ VarE "i302", l$ LitE 1]) $
                       l$ Ext $ LetLocE "l304" (AfterConstantLE 1 "lout301") $
                       l$ LetE ("t318",[],ProdTy [IntTy, PackedTy "Tree" "l304"],
                                l$ AppE "buildTreeSum" ["l304"] (l$ VarE "i303")) $
                       l$ LetE ("i309",[],IntTy, l$ ProjE 0 (l$ VarE "t318")) $
                       l$ LetE ("x305",[],PackedTy "Tree" "l304", l$ ProjE 1 (l$ VarE "t318")) $
                       l$ Ext $ LetLocE "l306" (AfterVariableLE "x305" "l304") $
                       l$ LetE ("t319",[],ProdTy [IntTy, PackedTy "Tree" "l306"],
                                l$ AppE "buildTreeSum" ["l306"] (l$ VarE "i303")) $
                       l$ LetE ("i310",[],IntTy, l$ ProjE 0 (l$ VarE "t319")) $
                       l$ LetE ("y307",[],PackedTy "Tree" "l306", l$ ProjE 1 (l$ VarE "t319")) $
                       l$ LetE ("j311",[],IntTy, l$ PrimAppE AddP [l$ VarE "i309", l$ VarE "i310"]) $
                       l$ LetE ("a308",[],PackedTy "Tree" "lout301",
                                l$ DataConE "lout301" "Node" [l$ VarE "x305", l$ VarE "y307"]) $
                       l$ LetE ("b312",[], ProdTy [IntTy, PackedTy "Tree" "lout301"],
                                l$ MkProdE [l$ VarE "j311", l$ VarE "a308"]) $
                       l$ VarE "b312")


buildTreeSumMainExp :: L Exp2
buildTreeSumMainExp = l$ Ext $ LetRegionE (VarR "r313") $
                      l$ Ext $ LetLocE "l314" (StartOfLE (VarR "r313")) $
                      l$ LetE ("z315",[],ProdTy [IntTy, PackedTy "Tree" "l314"],
                               l$ AppE "buildTreeSum" ["l314"] (l$ LitE 3)) $
                      l$ VarE "z315"


buildTreeSumProg :: Prog
buildTreeSumProg = Prog ddtree (M.fromList [("buildTreeSum", buildTreeSumFun)]) (Just (buildTreeSumMainExp, ProdTy [IntTy, PackedTy "Tree" "l314"]))


--------------------------------------------------------------------------------

printTupMainExp :: L Exp2
printTupMainExp = l$ Ext $ LetRegionE (VarR "r325") $
                  l$ Ext $ LetLocE "l326" (StartOfLE (VarR "r325")) $
                  l$ LetE ("i327",[], IntTy, l$ LitE 42) $
                  l$ LetE ("x328",[], PackedTy "Tree" "l326",
                           l$ DataConE "l326" "Leaf" [l$ LitE 1]) $
                  l$ LetE ("t329",[], ProdTy [IntTy, PackedTy "Tree" "l326"],
                           l$ MkProdE [l$ VarE "i327", l$ VarE "x328"]) $
                  l$ VarE "t329"

printTupProg :: Prog
printTupProg = Prog ddtree M.empty (Just (printTupMainExp, ProdTy [IntTy, PackedTy "Tree" "l326"]))

--------------------------------------------------------------------------------

printTupMainExp2 :: L Exp2
printTupMainExp2 = l$ Ext $ LetRegionE (VarR "r400") $
                  l$ Ext $ LetLocE "l401" (StartOfLE (VarR "r400")) $
                  l$ LetE ("x402",[], PackedTy "Tree" "l401",
                           l$ AppE "buildTree" ["l401"] (l$ LitE 2)) $
                  l$ Ext $ LetLocE "l403" (AfterVariableLE "x402" "l401") $
                  l$ LetE ("y404",[], PackedTy "Tree" "l403",
                           l$ AppE "buildTree" ["l403"] (l$ LitE 1)) $
                  l$ LetE ("z405",[], ProdTy [PackedTy "Tree" "l401", PackedTy "Tree" "l403"],
                           l$ MkProdE [l$ VarE "x402", l$ VarE "y404"]) $
                  l$ VarE "z405"

printTupProg2 :: Prog
printTupProg2 = Prog ddtree (M.fromList [("buildTree", buildTreeFun)])
                (Just (printTupMainExp2,
                       ProdTy [PackedTy "Tree" "l401", PackedTy "Tree" "l403"]))

--------------------------------------------------------------------------------

{-

addTrees :: Tree -> Tree -> Tree
addTrees t1 t2 =
  case t1 of
    Leaf n1    -> case t2 of
                    Leaf n2 -> Leaf (n1 + n2)
                    Node l2 r2 -> error "expected leaf here"
    Node l1 r1 -> case t2 of
                    Leaf n2 -> error "expected node here"
                    Node l2 r2 -> Node (addTrees l1 l2) (addTrees r1 r2)
-}

addTreesFun :: FunDef
addTreesFun = FunDef "addTrees" addTreesTy "trees354" addTreesBod
  where
    addTreesTy :: ArrowTy Ty2
    addTreesTy = (ArrowTy
                  [LRM "lin351" (VarR "r350") Input,
                   LRM "lin352" (VarR "r350") Input,
                   LRM "lout353" (VarR "r350") Output]
                  (ProdTy [PackedTy "Tree" "lin351", PackedTy "Tree" "lin352"])
                  (S.empty)
                  (PackedTy "Tree" "lout353")
                  [])

    addTreesBod :: L Exp2
    addTreesBod = l$ LetE ("tree1",[],PackedTy "Tree" "lin351",
                           l$ ProjE 0 (l$ VarE "trees354")) $
                  l$ LetE ("tree2",[],PackedTy "Tree" "lin352",
                           l$ ProjE 1 (l$ VarE "trees354")) $
                  l$ CaseE (l$ VarE "tree1")
                  [("Leaf", [("n355","l356")],
                    l$ CaseE (l$ VarE "tree2")
                       [("Leaf",[("n357","l358")],
                         l$ LetE ("n358",[],IntTy,l$ PrimAppE AddP [l$ VarE "n355",l$ VarE "n357"]) $
                         l$ LetE ("x359",[],PackedTy "Tree" "lout353",
                                  l$ DataConE "lout353" "Leaf" [l$ VarE "n358"]) $
                         l$ VarE "x359"
                        )]
                   ),
                    ("Node", [("x360","l361"), ("y362","l363")],
                     l$ CaseE (l$ VarE "tree2")
                        [("Node", [("x364","l365"), ("y366","l367")],
                          l$ Ext $ LetLocE "l368" (AfterConstantLE 1 "lout353") $
                          l$ LetE ("tree3",[],ProdTy [PackedTy "Tree" "l361",
                                                      PackedTy "Tree" "l365"],
                                   l$ MkProdE [l$ VarE "x360", l$ VarE "x364"]) $
                          l$ LetE ("x369",[],PackedTy "Tree" "l368",
                                   l$ AppE "addTrees" ["l361","l365","l368"] (l$ VarE "tree3")) $
                          l$ Ext $ LetLocE "l370" (AfterVariableLE "x369" "l368") $
                          l$ LetE ("tree4",[],ProdTy [PackedTy "Tree" "l363",
                                                      PackedTy "Tree" "l367"],
                                   l$ MkProdE [l$ VarE "y362", l$ VarE "y366"]) $
                          l$ LetE ("y371",[],PackedTy "Tree" "l370",
                                   l$ AppE "addTrees" ["l363","l367","l370"] (l$ VarE "tree4")) $
                          l$ LetE ("z372",[],PackedTy "Tree" "lout353",
                                    l$ DataConE "lout353" "Node" [l$ VarE "x369", l$ VarE "y371"]) $
                          l$ VarE "z372"
                         )]
                    )]

addTreesMainExp :: L Exp2
addTreesMainExp = l$ Ext $ LetRegionE (VarR "r400") $
                  l$ Ext $ LetLocE "l401" (StartOfLE (VarR "r400")) $
                  l$ LetE ("x402",[], PackedTy "Tree" "l401",
                           l$ AppE "buildTree" ["l401"] (l$ LitE 2)) $
                  l$ Ext $ LetLocE "l403" (AfterVariableLE "x402" "l401") $
                  l$ LetE ("y404",[], PackedTy "Tree" "l403",
                           l$ AppE "buildTree" ["l403"] (l$ LitE 2)) $
                  l$ LetE ("z405",[], ProdTy [PackedTy "Tree" "l401", PackedTy "Tree" "l403"],
                           l$ MkProdE [l$ VarE "x402", l$ VarE "y404"]) $
                  l$ Ext $ LetRegionE (VarR "r405") $
                  l$ Ext $ LetLocE "l406" (StartOfLE (VarR "r405")) $
                  l$ LetE ("a407",[],PackedTy "Tree" "l406",
                           l$ AppE "addTrees" ["l401","l403","l406"] (l$ VarE "z405")) $
                  l$ VarE "a407"

addTreesProg :: Prog
addTreesProg = Prog ddtree (M.fromList [("addTrees", addTreesFun)
                                       ,("buildTree", buildTreeFun)])
                    (Just (addTreesMainExp, PackedTy "Tree" "l406"))

--------------------------------------------------------------------------------

testProdFun :: FunDef
testProdFun = FunDef "testprod" testprodTy "tup130" testprodBod
  where
    testprodTy = (ArrowTy
                  [LRM "lin131" (VarR "r132") Input, LRM "lout133" (VarR "r132") Output]
                  (ProdTy [(PackedTy "Tree" "lin131"), IntTy])
                  (S.empty)
                  (ProdTy [(PackedTy "Tree" "lout133"), IntTy])
                  [])
    testprodBod = l$ LetE ("t134",[], PackedTy "Tree" "lin131", l$ ProjE 0 (l$ VarE "tup130")) $
                  l$ LetE ("i135",[], IntTy, l$ ProjE 1 (l$ VarE "tup130")) $
                  l$ CaseE (l$ VarE "t134")
                  [("Leaf",[("n136","l137")],
                    l$ LetE ("v138",[],IntTy, l$ PrimAppE AddP [l$ VarE "n136", l$ LitE 1]) $
                    l$ LetE ("lf139",[],PackedTy "Tree" "lout133",
                            l$ DataConE "lout133" "Leaf" [l$ VarE "v138"]) $
                    l$ LetE ("tup148",[], ProdTy [PackedTy "Tree" "lout133", IntTy],
                       l$ MkProdE [l$ VarE "lf139", l$ VarE "i135"]) $
                    l$ VarE "tup148"
                   ),
                   ("Node",[("x140","l141"), ("y142","l143")],
                    l$ Ext $ LetLocE "l144" (AfterConstantLE 1 "lout133") $
                    l$ LetE ("tup145",[], ProdTy [PackedTy "Tree" "l144", IntTy],
                             l$ AppE "testprod" ["l141","l144"]
                             (l$ MkProdE [l$ VarE "x140", l$ VarE "i135"])) $

                    l$ LetE ("x149",[], PackedTy "Tree" "l144", l$ ProjE 0 (l$ VarE "tup145")) $
                    l$ Ext $ LetLocE "l146" (AfterVariableLE "x149" "l144") $
                    l$ LetE ("tup147",[], ProdTy [PackedTy "Tree" "l146", IntTy],
                            l$ AppE "testprod" ["l143","l146"]
                            (l$ MkProdE [l$ VarE "y142", l$ VarE "i135"])) $
                    l$ LetE ("y150",[], PackedTy "Tree" "l146", l$ ProjE 0 (l$ VarE "tup147")) $
                    l$ LetE ("node151",[], PackedTy "Tree" "lout133",
                            l$ DataConE "lout133" "Node" [l$ VarE "x149", l$ VarE "y150"]) $
                    l$ LetE ("tup152",[],ProdTy [PackedTy "Tree" "lout133", IntTy],
                            l$ MkProdE [l$ VarE "node151", l$ VarE "i135"]) $
                    l$ VarE "tup152")
                  ]

testProdProg :: Prog
testProdProg = Prog ddtree (M.fromList [("testprod", testProdFun)]) Nothing

--------------------------------------------------------------------------------

-- Meaningless program, just to test flattenL2
testFlattenProg :: Prog
testFlattenProg = Prog M.empty (M.fromList [("intAdd",intAddFun)]) $ Just (testFlattenBod, IntTy)
  where
    testFlattenBod :: L Exp2
    testFlattenBod =
      l$ Ext $ LetRegionE (VarR "_") $
      l$ Ext $ LetLocE "_" (StartOfLE (VarR "_")) $
      l$ Ext $ LetLocE "_" (AfterConstantLE 1 "_") $
      l$ LetE ("v170",[],IntTy,
               l$ LetE ("v171",[],IntTy,
                        l$ AppE "intAdd" []
                        (l$ MkProdE [l$ PrimAppE AddP [l$ LitE 40, l$ LitE 2],
                                     l$ PrimAppE SubP [l$ LitE 44, l$ LitE 2]])) $
                l$ VarE "v171") $
      l$ VarE "v170"

--------------------------------------------------------------------------------

-- sumUp + setEven example in L2
-- gensym starts at 500

stree :: DDefs Ty2
stree = fromListDD [DDef (toVar "STree")
                    [ ("Leaf",[(False,IntTy)])
                    , ("Inner",[ (False, IntTy)
                               , (False, IntTy) -- this should be a boolean.
                                                -- for now, 1 is true, 0 is false
                               , (False, PackedTy "STree" "l")
                               , (False, PackedTy "STree" "l")])
                    ]]

sumUpFun :: FunDef
sumUpFun = FunDef "sumUp" sumUpFunTy "tr1" sumUpFunBod
  where
    sumUpFunTy :: ArrowTy Ty2
    sumUpFunTy = (ArrowTy
                  [LRM "lin501" (VarR "r500") Input, LRM "lout502" (VarR "r500") Output]
                  (PackedTy "STree" "lin501")
                  (S.empty)
                  (PackedTy "STree" "lout502")
                  [])


    sumUpFunBod :: L Exp2
    sumUpFunBod = l$ CaseE (l$ VarE "tr1") $
      [ ("Leaf", [("n503","l504")],
          l$ LetE ("x505",[],PackedTy "STree" "lout502",
                   l$ DataConE "lout502" "Leaf" [l$ VarE "n503"]) $
          l$ VarE "x505")

      , ("Inner", [("i506","l507"),("b508","l509"),("x510","l511"),("y512","l513")],
         l$ Ext $ LetLocE "l514" (AfterConstantLE 1 "lout502") $
         l$ Ext $ LetLocE "l550" (AfterVariableLE "i506" "l514") $
         l$ Ext $ LetLocE "l551" (AfterVariableLE "b508" "l550") $
         l$ LetE ("x515",[],PackedTy "STree" "l551",
                   l$ AppE "sumUp" ["l511","l551"] (l$ VarE "x510")) $
         l$ Ext $ LetLocE "l516" (AfterVariableLE "x515" "l551") $
         l$ LetE ("y517",[],PackedTy "STree" "l516",
                  l$ AppE "sumUp" ["l513","l516"] (l$ VarE "y512")) $
         l$ LetE ("v518",[],IntTy, l$ AppE "valueSTree" ["l551"] (l$ VarE "x515")) $
         l$ LetE ("v519",[],IntTy, l$ AppE "valueSTree" ["l516"] (l$ VarE "y517")) $
         l$ LetE ("v520",[],IntTy, l$ PrimAppE AddP [l$ VarE "v518", l$ VarE "v519"]) $
         l$ LetE ("z521",[],PackedTy "STree" "lout502",
                  l$ DataConE "lout502" "Inner" [l$ VarE "v520", l$ VarE "b508",
                                                 l$ VarE "x515", l$ VarE "y517"]) $
         l$ VarE "z521"
        )]


valueSTreeFun :: FunDef
valueSTreeFun = FunDef "valueSTree" valueSTreeFunTy "tr522" valueSTreeFunBod
  where
    valueSTreeFunTy :: ArrowTy Ty2
    valueSTreeFunTy = (ArrowTy
                       [LRM "lin524" (VarR "r523") Input]
                       (PackedTy "STree" "lin524")
                       (S.empty)
                       (IntTy)
                       [])

    valueSTreeFunBod :: L Exp2
    valueSTreeFunBod = l$ CaseE (l$ VarE "tr522") $
      [ ("Leaf", [("n523","l524")],
          l$ VarE "n523")

      , ("Inner", [("i525","l526"),("b527","l528"),("x529","l530"),("y531","l532")],
         l$ VarE "i525"
      )]


buildSTreeFun :: FunDef
buildSTreeFun = FunDef "buildSTree" buildSTreeTy "i543" buildSTreeBod
  where
    buildSTreeTy :: ArrowTy Ty2
    buildSTreeTy = (ArrowTy
                    [LRM "lout541" (VarR "r540") Output]
                    (IntTy)
                    (S.empty)
                    (PackedTy "STree" "lout541")
                    [])

    buildSTreeBod :: L Exp2
    buildSTreeBod = l$ LetE ("b542",[], BoolTy, l$ PrimAppE EqIntP [l$ VarE "i543", l$ LitE 0]) $
                   l$ IfE (l$ VarE "b542")
                   (l$ DataConE "lout541" "Leaf" [l$ LitE 1])
                   (l$ LetE ("i548",[], IntTy, l$ PrimAppE SubP [l$ VarE "i543", l$ LitE 1]) $
                    l$ LetE ("i554",[], IntTy, l$ LitE 0) $
                    l$ LetE ("b555",[], IntTy, l$ LitE 0) $
                    l$ Ext $ LetLocE "l544" (AfterConstantLE 1 "lout541") $
                    l$ Ext $ LetLocE "l552" (AfterVariableLE "i554" "l544") $
                    l$ Ext $ LetLocE "l553" (AfterVariableLE "b555" "l552") $
                    l$ LetE ("x545",[],PackedTy "STree" "l553",
                             l$ AppE "buildSTree" ["l553"] (l$ VarE "i548")) $
                    l$ Ext $ LetLocE "l545" (AfterVariableLE "x545" "l553") $
                    l$ LetE ("y546",[],PackedTy "STree" "l545",
                             l$ AppE "buildSTree" ["l545"] (l$ VarE "i548")) $
                    l$ LetE ("a547",[],PackedTy "STree" "lout541",
                             l$ DataConE "lout541" "Inner" [l$ VarE "i554", l$ VarE "b555",
                                                            l$ VarE "x545", l$ VarE "y546"]) $
                    l$ VarE "a547")

sumUpMainExp :: L Exp2
sumUpMainExp = l$ Ext $ LetRegionE (VarR "r530") $
                  l$ Ext $ LetLocE "l531" (StartOfLE (VarR "r530")) $
                  l$ LetE ("x532",[], PackedTy "STree" "l531",
                           l$ AppE "buildSTree" ["l531"] (l$ LitE 2)) $
                  l$ Ext $ LetRegionE (VarR "r536") $
                  l$ Ext $ LetLocE "l537" (StartOfLE (VarR "r536")) $
                  l$ LetE ("z538",[],PackedTy "STree" "l537",
                           l$ AppE "sumUp" ["l531","l537"] (l$ VarE "x532")) $
                  l$ VarE "z538"

sumUpProg :: Prog
sumUpProg = Prog stree (M.fromList [("sumUp", sumUpFun)
                                   ,("valueSTree", valueSTreeFun)
                                   ,("buildSTree", buildSTreeFun)
                                   ])
            (Just (sumUpMainExp, PackedTy "STree" "l537"))
