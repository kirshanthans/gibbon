module Gibbon.Passes.ControlFlowGraph (generateCfg) where

import Data.Graph as G
import Data.Map as M

import Gibbon.Common
import Gibbon.L1.Syntax as L1 
import Prelude as P
import Control.Monad as Mo

-- Type to store the CFG's for each function that apprears in code.
-- We should use annotations form the front end to onlt contruct CFG's 
-- for functions that are annotated. 
type CFGfunctionMap = M.Map FunDef1 (G.Graph, G.Vertex -> (Exp1, Int, [Int]), Int -> Maybe G.Vertex)

-- For now make this return, the CFGFunctionMap 
-- But this should ideally return the Constraints 
-- which should then be passed to the constraint solver. 
generateCfg :: Prog1 -> PassM Prog1
generateCfg prg@Prog{ddefs, fundefs, mainExp} = do 
    let cfgFunctionMap = M.empty 
    newCfgFunctionMap <- generateCfgFunctions cfgFunctionMap (M.elems fundefs)
    let l1 = prg { ddefs = ddefs
               , fundefs = fundefs 
               , mainExp = mainExp
               }
    pure l1


generateCfgFunctions :: CFGfunctionMap -> [FunDef1] -> PassM CFGfunctionMap
generateCfgFunctions cfgMap defs = 
    case defs of 
        [] -> pure cfgMap 
        x:xs -> do 
            (cfgMapNew, edgeList) <- generateCfgFunction cfgMap x
            newMap <- generateCfgFunctions cfgMap xs 
            dbgTraceIt (sdoc edgeList) dbgTraceIt ("\n") dbgTraceIt ("\n") dbgTraceIt ("\n") pure newMap

        
        
generateCfgFunction :: CFGfunctionMap -> FunDef1 -> PassM (CFGfunctionMap, [(Exp1, Int, [Int])])
generateCfgFunction cfgMap f@FunDef { funName, funBody, funTy, funArgs } = do  
    (edgeList, succ) <- generateCFGExp 0 funBody 
         -- .... update the map with the cfg of the new function. 
    pure (cfgMap, edgeList)


generateCFGExp :: Int -> Exp1 -> PassM ( [(Exp1, Int, [Int])] , Int )
generateCFGExp vertexCounter exp1 = case exp1 of 
    --Recursively do for args, for now assuming this is a leaf node (base case)
    DataConE loc dcon args -> do 
        let edge = (exp1, vertexCounter, [])
        pure ([edge], vertexCounter)
    VarE{} -> do 
        let edge = (exp1, vertexCounter, [])
        pure ([edge], vertexCounter)
    LitE{} -> do 
        let edge = (exp1, vertexCounter, [])
        pure ([edge], vertexCounter)
    CharE{} -> do 
        let edge = (exp1, vertexCounter, [])
        pure ([edge], vertexCounter)
    FloatE{} -> do 
        let edge = (exp1, vertexCounter, [])
        pure ([edge], vertexCounter)
    LitSymE{} -> do 
        let edge = (exp1, vertexCounter, [])
        pure ([edge], vertexCounter)
    AppE f locs args -> do 
        let vertices = P.take (length args) (iterate (+1) (vertexCounter+1))
        results <- Mo.zipWithM generateCFGExp vertices args 
        let succList = P.map (\x -> snd x) results 
        let edge     = ( (VarE f), vertexCounter, succList)
        let succEdges = concat (P.map (\x -> fst x) results)
        let newEdges = succEdges ++ [edge]
        pure (newEdges, vertexCounter)
    PrimAppE f args -> do 
        let vertices = P.take (length args) (iterate (+1) (vertexCounter+1))
        results <- Mo.zipWithM generateCFGExp vertices args
        let succList = P.map (\x -> snd x) results
        let edge     = (exp1, vertexCounter, succList)
        let succEdges = concat (P.map (\x -> fst x) results)
        let newEdges = succEdges ++ [edge]
        pure (newEdges, vertexCounter)
    LetE (v,loc,ty,rhs) bod -> do 
        (edgeList, succ) <- generateCFGExp (vertexCounter+1) bod
        let exp'  = LetE (v, loc, ty, rhs) $ VarE v
        let edge = (exp', vertexCounter, [succ])
        let edgeList' = edgeList ++ [edge]
        {-dbgTraceIt (sdoc exp)-} 
        pure (edgeList', vertexCounter)
    CaseE scrt mp -> do 
        let vertices = P.take (length mp) (iterate (+2) (vertexCounter+1) )
        --results <- Mo.zipWithM generateCFGExp vertices ( P.map (\a -> thd3 a) mp)
        --let succList = P.map (\x -> snd x) results
        --let edge     = (scrt, vertexCounter, succList)
        --let succEdges = concat (P.map (\x -> fst x) results)
        --let newEdges = succEdges ++ [edge]
        --pure (newEdges, vertexCounter)
        results <- Mo.zipWithM generateVerticesCase vertices mp 
        let succList = P.map (\x -> snd x) results
        let edge     = (scrt, vertexCounter, succList)
        let succEdges = concat (P.map (\x -> fst x) results)
        let newEdges  = succEdges ++ [edge]
        pure (newEdges, vertexCounter)
    IfE a b c -> do 
        (edgeListB, succB) <- generateCFGExp (vertexCounter+1) b 
        (edgeListC, succC) <- generateCFGExp (vertexCounter+2) c 
        let succList = [succB, succC]
        let edge     = (a, vertexCounter, succList)
        let newEdges = edgeListB ++ edgeListC ++ [edge]
        pure (newEdges, vertexCounter)
    MkProdE xs -> do 
        let vertices = P.take (length xs) (iterate (+1) (vertexCounter+1) )
        results <- Mo.zipWithM generateCFGExp vertices xs
        let succList  = P.map (\x -> snd x) results
        let edge      = (exp1, vertexCounter, succList)
        let succEdges = concat (P.map (\x -> fst x) results)
        let newEdges  = succEdges ++ [edge]
        pure (newEdges, vertexCounter)
    ProjE i e -> error "ControlFlowGraph: TODO ProjE"
    TimeIt e ty b -> error "ControlFlowGraph: TODO TimeIt"
    WithArenaE v e -> error "ControlFlowGraph: TODO WithArenaE"
    SpawnE f locs args -> error "ControlFlowGraph: TODO SpawnE"
    SyncE   -> error "ControlFlowGraph: TODO SyncE"
    Ext _   -> error "ControlFlowGraph: TODO Ext"
    MapE{}  -> error "ControlFlowGraph: TODO MapE"
    FoldE{} -> error "ControlFlowGraph: TODO FoldE"


-- generateVerticesCase' :: Int -> [(DataCon, [(Var, loc)] , Exp1)] -> PassM ( [(Exp1, Int, [Int])] , Int)
-- generateVerticesCase' inVal list = case list of
--     [] -> pure ([], inVal) 
--     x:xs -> do 
--     let (edges, succ) <- generateVerticesCase inVal x 
--     let (edgesrst, succrst) <- generateVerticesCase' (inVal+1) xs
--     let edgeslist' = edges ++ edgesrst



generateVerticesCase :: Int -> (DataCon, [(Var, loc)] , Exp1) -> PassM ( [(Exp1, Int, [Int])] , Int )
generateVerticesCase currVertex branch = do 
    let datacon      = fst3 branch 
    let fields_locs  = snd3 branch
    let fields       = P.map (\x -> ( VarE (fst x) )) fields_locs
    let dataconExp   = DataConE () datacon fields
    (edgeList, succ) <- generateCFGExp (currVertex+1) (thd3 branch) 
    let edge = (dataconExp, currVertex, [succ])
    let newEdges = edgeList ++ [edge]
    pure (newEdges, currVertex) 
    