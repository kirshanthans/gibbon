module Gibbon.Passes.ControlFlowGraph (generateCfg) where

import Data.Graph as G
import Data.Map as M
import Data.Set as S
import Data.List as L

import Gibbon.Common
import Gibbon.L1.Syntax as L1 
import Prelude as P
import Control.Monad as Mo

import Data.Maybe as Mb

{-
Type CFGfunctionMap: Mapping from function definition, to the control flow graph of the program. 
Edge : A tuple of expression and its likelihood. 
See Data.Graph in containers for more definitions. 
TODO: The functions for which the CFG should be annoted at the front-end level and they should be passable to this pass. 
Only generate CFG for functions which are annotated. 
-}
type CFGfunctionMap = M.Map FunDef1 (G.Graph, G.Vertex -> ( (Exp1, Int), Int, [Int]), Int -> Maybe G.Vertex)

{- Type VariableMap: Stores mapping from Variable to wheather it comes from a particular datacon. -}
type VariableMap = M.Map Var (Maybe (DataCon, Int))

{- Store the field graphs for each function-}
type FieldMap = M.Map FunDef1 (G.Graph, G.Vertex -> ((DataCon, Int), Int, [Int]), Int -> Maybe G.Vertex)


-- This is a PassM Prog1 just for testing purposes. So that is can be called in the pass pipeline. 
-- This should just be exportable as a function. ShuffleDataCon pass should call this pass eventually; Togehter with the constraint solver. 
generateCfg :: Prog1 -> PassM Prog1
generateCfg prg@Prog{ddefs, fundefs, mainExp} = do 
    let cfgFunctionMap = M.empty 
    newCfgFunctionMap <- generateCfgFunctions cfgFunctionMap (M.elems fundefs)
    let l1 = prg { ddefs = ddefs
               , fundefs = fundefs 
               , mainExp = mainExp
               }
    pure l1


{- Takes a map, list of function definitions, return update map with CFG for each funciton in the list -}
generateCfgFunctions :: CFGfunctionMap -> [FunDef1] -> PassM CFGfunctionMap
generateCfgFunctions cfgMap defs = 
    case defs of 
        [] -> pure cfgMap 
        x:xs -> do 
            (cfgMapNew, edgeList) <- generateCfgFunction cfgMap x
            newMap <- generateCfgFunctions cfgMap xs 
            {-dbgTraceIt (sdoc edgeList) dbgTraceIt ("\n") dbgTraceIt ("\n") dbgTraceIt ("\n")-}
            pure newMap
        
        
{- Generate a CFG for the corresponsing function -}            
generateCfgFunction :: CFGfunctionMap -> FunDef1 -> PassM (CFGfunctionMap, [((Exp1, Int) , Int, [Int])])
generateCfgFunction cfgMap f@FunDef { funName, funBody, funTy, funArgs } = do  
    (edgeList, succ, maxDepth) <- generateCFGExp 0 100 funBody
    let (graph, nodeFromVertex, vertexFromKey) = G.graphFromEdges edgeList
    let x  = topSort graph
    let x' = P.map nodeFromVertex x
    let datacon :: String = "Blog"
    let map = backtrackVariablesToDataConFields x'
    let edges = constructFieldGraph Nothing nodeFromVertex vertexFromKey x' x' map datacon   
    -- dbgTraceIt (sdoc varList) dbgTraceIt ("\n") dbgTraceIt (sdoc varList') dbgTraceIt ("\n") 
    -- dbgTraceIt (sdoc x') dbgTraceIt ("\n") dbgTraceIt (sdoc map) dbgTraceIt ("\n") 
    -- pure (cfgMap, edgeList)
    -- dbgTraceIt (sdoc x) dbgTraceIt (sdoc x') dbgTraceIt ("\n") 
    dbgTraceIt (sdoc edges) dbgTraceIt ("\n") pure (cfgMap, edgeList)


generateCFGExp :: Int -> Int -> Exp1 -> PassM ( [ ((Exp1, Int) , Int, [Int]) ] , Int, Int)
generateCFGExp vertexCounter edgeWeight exp1 = case exp1 of 
    -- Recursively do for args? for now assuming this is a leaf node (base case)
    -- In the future we should have a clear differentiation between a case binding that introduces variables that can be read.
    -- versus a let binding where a Data Constructure is being written and its fields are being materialized. 
    DataConE loc dcon args -> do 
        let edge = ( (exp1, edgeWeight), vertexCounter, [] )
        pure ([edge], vertexCounter, vertexCounter)
    VarE{} -> do
        let edge = ( (exp1, edgeWeight), vertexCounter, [] ) 
        pure ([edge], vertexCounter, vertexCounter)
    LitE{} -> do 
        let edge = ( (exp1, edgeWeight), vertexCounter, [] )
        pure ([edge], vertexCounter, vertexCounter)
    CharE{} -> do 
        let edge = ( (exp1, edgeWeight) , vertexCounter, [] )
        pure ([edge], vertexCounter, vertexCounter)
    FloatE{} -> do 
        let edge = ( (exp1, edgeWeight) , vertexCounter, [] )
        pure ([edge], vertexCounter, vertexCounter)
    LitSymE{} -> do 
        let edge = ( (exp1, edgeWeight) , vertexCounter, [])
        pure ([edge], vertexCounter, vertexCounter)
    AppE f locs args -> do
        (edgeList, succList, maxDepth) <- processExpListSeq (vertexCounter+1) edgeWeight args
        let edge     = ( ((VarE f), edgeWeight) , vertexCounter, succList )
        let newEdges = edgeList ++ [edge]
        pure (newEdges, vertexCounter, maxDepth)
    PrimAppE f args -> do 
        (edgeList, succList, maxDepth) <- processExpListSeq (vertexCounter+1) edgeWeight args
        let edge     = ( (exp1, edgeWeight) , vertexCounter, succList )
        let newEdges = edgeList ++ [edge]
        pure (newEdges, vertexCounter, maxDepth)
    LetE (v,loc,ty,rhs) bod -> do 
        (edgeList, succ, maxDepth) <- generateCFGExp (vertexCounter+1) edgeWeight bod
        let exp'  = LetE (v, loc, ty, rhs) $ VarE v
        let edge = ( (exp', edgeWeight) , vertexCounter, [succ] )
        let edgeList' = edgeList ++ [edge]
        pure (edgeList', vertexCounter, maxDepth)
    CaseE scrt mp -> do 
        (edgeList, succList, maxDepth) <- processExpSeqCase (vertexCounter+1) (edgeWeight `div` (P.length mp)) mp
        let edge     = ( (scrt, edgeWeight) , vertexCounter, succList )
        let newEdges  = edgeList ++ [edge]
        pure (newEdges, vertexCounter, maxDepth)
    IfE a b c -> do 
        (edgeListB, succB, d1) <- generateCFGExp (vertexCounter+1) (edgeWeight `div` 2) b 
        (edgeListC, succC, d2) <- generateCFGExp (d1+1) (edgeWeight `div` 2) c 
        let succList = [succB, succC]
        let edge     = ( (a, edgeWeight) , vertexCounter, succList )
        let newEdges = edgeListB ++ edgeListC ++ [edge]
        pure (newEdges, vertexCounter, P.maximum [d1, d2])
    MkProdE xs -> do 
        (edgeList, succList, maxDepth) <- processExpListSeq (vertexCounter+1) edgeWeight xs
        let edge      = ( (exp1, edgeWeight) , vertexCounter, succList )
        let newEdges  = edgeList ++ [edge]
        pure (newEdges, vertexCounter, maxDepth)
    ProjE i e -> error "generateCFGExp: TODO ProjE"
    TimeIt e ty b -> error "generateCFGExp: TODO TimeIt"
    WithArenaE v e -> error "generateCFGExp: TODO WithArenaE"
    SpawnE f locs args -> error "generateCFGExp: TODO SpawnE"
    SyncE   -> error "generateCFGExp: TODO SyncE"
    Ext _   -> error "generateCFGExp: TODO Ext"
    MapE{}  -> error "generateCFGExp: TODO MapE"
    FoldE{} -> error "generateCFGExp: TODO FoldE"

{-
Process a list of expressions sequentially rather than in parallel as it would be though a Map 
Makes it much easier to thread, vertex id's and likelihoods. 
-}
processExpListSeq :: Int -> Int -> [Exp1] -> PassM ([((Exp1, Int), Int, [Int])] , [Int], Int)
processExpListSeq currVertex edgeWeight exp = case exp of 
    []   -> pure ([], [], currVertex)
    x:xs -> do 
        (edgeList, succ, maxDepth) <- generateCFGExp currVertex edgeWeight x
        (edgeList', succ', maxDepth') <- processExpListSeq (maxDepth+1) edgeWeight xs
        let newEdgeList = edgeList ++ edgeList'
        let succList    = [succ] ++ succ'
        pure (newEdgeList, succList, maxDepth') 

{-
Process list of case expressions sequentially. 
-}
processExpSeqCase :: Int -> Int -> [(DataCon, [(Var, loc)] , Exp1)] -> PassM ( [((Exp1, Int), Int, [Int])] , [Int], Int )
processExpSeqCase currVertex edgeWeight lst = case lst of 
    [] -> pure ([], [], currVertex)
    x:xs -> do 
        (edgeList, succ, maxDepth) <- generateVerticesCase currVertex edgeWeight x 
        (edgeList', succList, maxDepth') <- processExpSeqCase (maxDepth+1) edgeWeight xs
        let newEdgeList = edgeList ++ edgeList' 
        let succList'    = [succ] ++ succList
        pure (newEdgeList, succList', maxDepth')

{-
Helper function to generate a Vertex for each case binding. 
-}
generateVerticesCase :: Int -> Int -> (DataCon, [(Var, loc)] , Exp1) -> PassM ( [((Exp1, Int) , Int, [Int])] , Int, Int )
generateVerticesCase currVertex edgeWeight branch = do 
    let datacon      = fst3 branch 
    let fields_locs  = snd3 branch
    let fields       = P.map (\x -> ( VarE (fst x) )) fields_locs
    let dataconExp   = DataConE () datacon fields
    (edgeList, succ, maxDepth) <- generateCFGExp (currVertex+1) edgeWeight (thd3 branch) 
    let edge = ((dataconExp, edgeWeight) , currVertex, [succ])
    let newEdges = edgeList ++ [edge]
    pure (newEdges, currVertex, maxDepth) 
    

backtrackVariablesToDataConFields :: [((Exp1, Int) , Int, [Int])] -> VariableMap
backtrackVariablesToDataConFields graph = case graph of 
    [] -> M.empty -- No variable to process. 
    x:xs -> let newMap  = processVertex graph x M.empty
                mlist   = M.toList (newMap)
                m       = backtrackVariablesToDataConFields xs
                mlist'  = M.toList m
                newMap' = M.fromList (mlist ++ mlist')
             in newMap'


processVertex :: [((Exp1, Int) , Int, [Int])] -> ((Exp1, Int) , Int, [Int]) -> VariableMap -> VariableMap
processVertex graph node map = case node of 
    ((expression, likelihood) , id, succ) -> case expression of
                                                    DataConE loc dcon args -> let freeVariables = L.concat (P.map (\x -> S.toList (gFreeVars x)) args)
                                                                                  maybeIndexes  = P.map (getDataConIndexFromVariable graph) freeVariables 
                                                                                  mapList       = M.toList map 
                                                                                  newMapList    = P.zipWith (\x y -> (x, y)) freeVariables maybeIndexes
                                                                                in M.fromList (mapList ++ newMapList)

                                                    _                      -> map


getDataConIndexFromVariable :: [((Exp1, Int) , Int, [Int])] -> Var -> Maybe (DataCon, Int)
getDataConIndexFromVariable graph variable = case graph of 
         [] -> Nothing 
         x:xs -> let status = compareVariableWithDataConFields x variable
                   in case status of 
                       Nothing -> getDataConIndexFromVariable xs variable
                       Just val -> Just val 

compareVariableWithDataConFields :: ((Exp1, Int), Int, [Int]) -> Var -> Maybe (DataCon, Int)
compareVariableWithDataConFields node variable = case node of 
    ((exp, likelihood) , id, _) -> case exp of 
        DataConE loc dcon args -> let variables = [var | VarE var <- args]
                                      results   = P.map (variable ==) variables
                                      maybeIndex = L.elemIndex True results
                                    in case maybeIndex of 
                                            Nothing  -> Nothing 
                                            Just val -> Just (dcon, val) 
        _ -> Nothing




{- Return the freeVariables bound by an expression in Order -}
freeVarsCFG2 :: Exp1 -> [Var]
freeVarsCFG2 exp = case exp of
    DataConE loc dcon args -> [] 
    VarE var -> [var]
    LitE val -> []
    CharE char -> []
    FloatE val -> []
    LitSymE var -> [var]
    AppE f locs args -> let var_list_list = P.map (freeVarsCFG2) args
                            var_list      = L.concat var_list_list
                         in var_list
                              
    PrimAppE f args -> let var_list_list = P.map (freeVarsCFG2) args
                           var_list      = L.concat var_list_list
                         in var_list

    LetE (v,loc,ty,rhs) bod -> freeVarsCFG2 rhs 
    -- CaseE scrt mp -> not there in cfg node
    -- IfE a b c -> not there in cfg node
    MkProdE xs -> let var_list_list = P.map (freeVarsCFG2) xs
                      var_list      = L.concat var_list_list
                    in var_list

    ProjE i e -> error "freeVarsCFG2: TODO ProjE"
    TimeIt e ty b -> error "freeVarsCFG2: TODO TimeIt"
    WithArenaE v e -> error "freeVarsCFG2: TODO WithArenaE"
    SpawnE f locs args -> error "freeVarsCFG2: TODO SpawnE"
    SyncE   -> error "freeVarsCFG2: TODO SyncE"
    Ext _   -> error "freeVarsCFG2: TODO Ext"
    MapE{}  -> error "freeVarsCFG2: TODO MapE"
    FoldE{} -> error "freeVarsCFG2: TODO FoldE" 




removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates list = case list of 
                                []   -> []
                                a:as -> a:removeDuplicates (P.filter (/=a) as)


{-
From a given graph generate the Field ordering subgraph.
A subgraph that only contains Fields from the dataCons as Vertices. 
Edges amongst vertices amount to the READ ACCESS Patterns amongs the fields of the DataCon. 
For now, we only cares about read <-> read dependencies. 

RETURN: an edge list and corresponding weight of the the edges
Edge: a tuple from vertex to vertex, left dominates right. 

TODO: any FIXMEs in the function. 

a.) Multiple datacon fields read in the same expression. 
    Since this will be run after flatten, it is safe to assume that only possibly a maximum of two variables can be read in one let binding. Except function calls! where more than two fields can be passed as arguments. 
-}

evaluateExpressionFieldGraph :: Maybe (DataCon, Int) -> (G.Vertex -> ( (Exp1, Int) , Int, [Int])) -> (Int -> Maybe G.Vertex) -> [((Exp1, Int) , Int, [Int])] -> [((Exp1, Int) , Int, [Int])] -> VariableMap -> DataCon -> [ Var ] -> [Int] -> Int -> [ ( (Int, Int) , Int ) ]
evaluateExpressionFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon freeVars successors likelihood = case currField of 
    Nothing         -> let fromDataCon'   = P.map (\v -> M.findWithDefault Nothing v map) (removeDuplicates freeVars)
                           justDcons      = [Just x | Just x <- fromDataCon']
                           fromDataCon''  = if P.null justDcons then [Nothing] else justDcons                        
                         in case fromDataCon'' of 
                                    [ a ] -> case a of 
                                                 Nothing         -> [ ] ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon
                                                 Just (dcon, id) -> case (dcon == datacon) of 
                                                                            True -> let succ'         = Mb.catMaybes $ P.map vertexFromNode successors
                                                                                        succVertices  = P.map nodeFromVertex succ'
                                                                                        succExp       = P.map (\x -> (fst . fst3) x) succVertices
                                                                                        succprob      = P.map (\x -> (snd . fst3) x) succVertices
                                                                                        {- list of list, where each list stores variables -}
                                                                                        succDataCon   = P.map (\x -> findFieldInDataConFromVariableInExpression x graph map datacon) succExp 
                                                                                        {- list of tuples, where each tuple == ([(dcon, id), ... ], likelihood)    -}
                                                                                        succDataCon'  = P.zipWith (\x y -> (x, y)) succDataCon succprob 
                                                                                        newEdges      = P.concat $ P.map (\x -> case x of 
                                                                                                                           (varsl, prob) -> P.map (\y -> ( (id, snd y) , prob ) ) varsl       
                                                                                                                         ) succDataCon'  
                                                                                      in case newEdges of 
                                                                                            [] -> case successors of 
                                                                                                     [] -> [] ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon
                                                                                                     _  -> newEdges ++ constructFieldGraph (Just (dcon, id)) nodeFromVertex vertexFromNode graph xs map datacon
                                                                
                                                                                            _  -> newEdges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon

                                                                            _       -> [] ++ constructFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon



                                    _     -> error "evaluateExpressionFieldGraph: More than one variable from DataCon in a let binding not modelled into Field dependence graph yet!" 




    Just (dcon, pred) -> let fromDataCon'   = P.map (\v -> M.findWithDefault Nothing v map) (removeDuplicates freeVars) 
                             justDcons      = [Just x | Just x <- fromDataCon']
                             fromDataCon''  = if P.null justDcons then [Nothing] else justDcons
                          in case fromDataCon'' of 
                                    [ a ] -> case a of 
                                                 Nothing -> let  succ'         = Mb.catMaybes $ P.map vertexFromNode successors
                                                                 succVertices  = P.map nodeFromVertex succ'
                                                                 succExp       = P.map (\x -> (fst . fst3) x) succVertices
                                                                 succprob      = P.map (\x -> (snd . fst3) x) succVertices
                                                                 {- list of list, where each list stores variables -}
                                                                 succDataCon   = P.map (\x -> findFieldInDataConFromVariableInExpression x graph map datacon) succExp 
                                                                 {- list of tuples, where each tuple == ([(dcon, id), ... ], likelihood)    -}
                                                                 succDataCon'  = P.zipWith (\x y -> (x, y)) succDataCon succprob 
                                                                 newEdges      = P.concat $ P.map (\x -> case x of 
                                                                                                     (varsl, prob) -> P.map (\y -> ( (pred, snd y) , prob ) ) varsl       
                                                                                                  ) succDataCon'  
                                                             in case newEdges of 
                                                                        [] -> case successors of 
                                                                                      [] -> [] ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon
                                                                                      _  -> newEdges ++ constructFieldGraph (Just (dcon, pred)) nodeFromVertex vertexFromNode graph xs map datacon
                                                                        
                                                                        _  -> newEdges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon




                                                 Just (dcon', id') -> case (dcon' == datacon) of 
                                                                             True -> let edges = [((pred, id'), likelihood)]
                                                                                         succ'         = Mb.catMaybes $ P.map vertexFromNode successors
                                                                                         succVertices  = P.map nodeFromVertex succ'
                                                                                         succExp       = P.map (\x -> (fst . fst3) x) succVertices
                                                                                         succprob      = P.map (\x -> (snd . fst3) x) succVertices
                                                                                         succDataCon   = P.map (\x -> findFieldInDataConFromVariableInExpression x graph map datacon) succExp 
                                                                                         succDataCon'  = P.zipWith (\x y -> (x, y)) succDataCon succprob 
                                                                                         newEdges      = P.concat $ P.map (\x -> case x of 
                                                                                                     (varsl, prob) -> P.map (\y -> ( (pred, snd y) , prob ) ) varsl       
                                                                                                  ) succDataCon' 
                                                                                       in newEdges ++ edges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon
                                                                                             
                                                                                           
                                                                                       
                                                                                       
                                                                             _    -> [] ++ constructFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon

                                    _     -> error "evaluateExpressionFieldGraph: More than one variable from DataCon in a let binding not modelled into Field dependence graph yet!"



constructFieldGraph :: Maybe (DataCon, Int) -> (G.Vertex -> ( (Exp1, Int) , Int, [Int])) -> (Int -> Maybe G.Vertex) -> [((Exp1, Int) , Int, [Int])] -> [((Exp1, Int) , Int, [Int])] -> VariableMap -> DataCon -> [ ( (Int, Int) , Int ) ]
constructFieldGraph currField nodeFromVertex vertexFromNode graph progress map datacon = case progress of 
               [] -> [] 
               x:xs -> let ((exp, likelihood) , id'', successors) = x
                         in case exp of 
                            LitE val               -> case successors of 
                                                            [] -> []  ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon
                                                            _  -> []  ++ constructFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon
                            CharE char             -> case successors of 
                                                            [] -> []  ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon
                                                            _  -> []  ++ constructFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon
                            FloatE val             -> case successors of 
                                                            [] -> []  ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon
                                                            _  -> []  ++ constructFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon
                            DataConE loc dcon args -> case successors of 
                                                             [] -> []  ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon
                                                             _  -> []  ++ constructFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon

                            VarE var -> evaluateExpressionFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon [var] successors likelihood

                            LitSymE var -> evaluateExpressionFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon [var] successors likelihood

                            LetE (v,loc,ty,rhs) bod -> evaluateExpressionFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon (freeVarsCFG2 rhs) successors likelihood

                            AppE f locs args  -> evaluateExpressionFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon (freeVarsCFG2 exp) successors likelihood
        
                            PrimAppE f args  -> evaluateExpressionFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon (freeVarsCFG2 exp) successors likelihood

                            MkProdE xss -> evaluateExpressionFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon (freeVarsCFG2 exp) successors likelihood

                            ProjE i e -> error "constructFieldGraph: TODO ProjE"
                            TimeIt e ty b -> error "constructFieldGraph: TODO TimeIt"
                            WithArenaE v e -> error "constructFieldGraph: TODO WithArenaE"
                            SpawnE f locs args -> error "constructFieldGraph: TODO SpawnE"
                            SyncE   -> error "constructFieldGraph: TODO SyncE"
                            Ext _   -> error "constructFieldGraph: TODO Ext"
                            MapE{}  -> error "constructFieldGraph: TODO MapE"
                            FoldE{} -> error "constructFieldGraph: TODO FoldE" 

{- 
From an expression provided, Recursively find all the variables that come from a DataCon expression, that is, are fields in a DataConE.  
-}
findFieldInDataConFromVariableInExpression :: Exp1 -> [((Exp1, Int) , Int, [Int])] -> VariableMap -> DataCon -> [(DataCon, Int)]
findFieldInDataConFromVariableInExpression exp graph map datacon = case exp of
    VarE var -> let fromDataCon  = M.findWithDefault Nothing var map
                  in case fromDataCon of 
                       Nothing -> [] 
                       Just (dcon, id')  -> if dcon == datacon then [(dcon, id')] else []

    LitSymE var -> let fromDataCon = M.findWithDefault Nothing var map
                    in case fromDataCon of 
                       Nothing -> [] 
                       Just (dcon, id')  -> if dcon == datacon then [(dcon, id')] else []
                                            

    LetE (v,loc,ty,rhs) bod -> let freeVars = freeVarsCFG2 rhs 
                                   fromDataCon  = P.map (\v -> M.findWithDefault Nothing v map) freeVars 
                                   removeMaybe  = Mb.catMaybes fromDataCon 
                                   newDatacons  = [ if dcon == datacon then Just (dcon, id') else Nothing | (dcon, id') <- removeMaybe ]
                                   newDatacons' = Mb.catMaybes newDatacons
                                 in newDatacons'

    AppE f locs args  ->  let freeVars = freeVarsCFG2 exp 
                              fromDataCon = P.map (\v -> M.findWithDefault Nothing v map) freeVars
                              removeMaybe  = Mb.catMaybes fromDataCon 
                              newDatacons  = [ if dcon == datacon then Just (dcon, id') else Nothing | (dcon, id') <- removeMaybe ]
                              newDatacons' = Mb.catMaybes newDatacons
                            in newDatacons' 
                            
    PrimAppE f args  ->  let freeVars =  freeVarsCFG2 exp 
                             fromDataCon = P.map (\v -> M.findWithDefault Nothing v map) freeVars
                             removeMaybe  = Mb.catMaybes fromDataCon 
                             newDatacons  = [ if dcon == datacon then Just (dcon, id') else Nothing | (dcon, id') <- removeMaybe ]
                             newDatacons' = Mb.catMaybes newDatacons
                            in newDatacons'

    LitE val -> []   
    CharE char -> [] 
    FloatE val -> []  

    DataConE loc dcon args -> [] 

    MkProdE xss -> let freeVars =  freeVarsCFG2 exp 
                       fromDataCon = P.map (\v -> M.findWithDefault Nothing v map) freeVars
                       removeMaybe  = Mb.catMaybes fromDataCon 
                       newDatacons  = [ if dcon == datacon then Just (dcon, id') else Nothing | (dcon, id') <- removeMaybe ]
                       newDatacons' = Mb.catMaybes newDatacons
                     in newDatacons' 

    ProjE i e -> error "findFieldInDataConFromVariableInExpression: TODO ProjE"
    TimeIt e ty b -> error "findFieldInDataConFromVariableInExpression: TODO TimeIt"
    WithArenaE v e -> error "findFieldInDataConFromVariableInExpression: TODO WithArenaE"
    SpawnE f locs args -> error "findFieldInDataConFromVariableInExpression: TODO SpawnE"
    SyncE   -> error "findFieldInDataConFromVariableInExpression: TODO SyncE"
    Ext _   -> error "findFieldInDataConFromVariableInExpression: TODO Ext"
    MapE{}  -> error "findFieldInDataConFromVariableInExpression: TODO MapE"
    FoldE{} -> error "findFieldInDataConFromVariableInExpression: TODO FoldE" 