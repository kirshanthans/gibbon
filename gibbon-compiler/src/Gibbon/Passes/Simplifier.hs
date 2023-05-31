{-# LANGUAGE FlexibleContexts #-}

module Gibbon.Passes.Simplifier ( simplifyL1, lateInlineTriv, simplifyLocBinds ) where

import Data.Functor.Foldable as Foldable
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L ( isPrefixOf )

import Gibbon.Common
import Gibbon.Language
import Gibbon.L1.Syntax
import Gibbon.L2.Syntax
import qualified Gibbon.L4.Syntax as L4
import Gibbon.Passes.Freshen (freshNames1, freshFun1)

import Data.Bifunctor (second)
import Data.Maybe (fromMaybe)

--------------------------------------------------------------------------------

markRecFns :: Prog1 -> PassM Prog1
markRecFns (Prog ddefs fundefs main) = do
    let fundefs' = M.map
                     (\fn@FunDef{funName,funBody,funMeta} ->
                          if funName `S.member` gFreeVars funBody
                          then fn { funMeta = funMeta { funRec = Rec } }
                          else fn)
                     fundefs
    pure (Prog ddefs fundefs' main)

inlineFuns :: Prog1 -> PassM Prog1
inlineFuns (Prog ddefs fundefs main) = do
    main' <- case main of
               Nothing -> pure Nothing
               Just (e,ty) -> do
                 e' <- cataM go e
                 pure $ Just (e', ty)
    fundefs' <- mapM (\fn -> do
                           bod <- cataM go (funBody fn)
                           pure $ fn { funBody = bod})
                     fundefs
    pure (Prog ddefs fundefs' main')
  where
    go :: PreExpF E1Ext () (UrTy ()) Exp1 -> PassM Exp1
    go ex =
      case ex of
        AppEF f [] args -> do
            let fn = fundefs M.! f
            if funInline (funMeta fn) == Inline && funRec (funMeta fn) == NotRec
              then do
                FunDef{funArgs,funTy,funBody} <- freshFun1 fn
                let in_tys = fst funTy
                    binds = map (\(v,ty,e) -> (v,[],ty,e)) (zip3 funArgs in_tys args)
                pure $ mkLets binds funBody
              else do
                args' <- mapM (go . project) args
                pure $ AppE f [] args'
        _ -> pure $ embed ex

deadFunElim :: Prog1 -> PassM Prog1
deadFunElim (Prog ddefs fundefs main) = do
    let used = case main of
                 Nothing -> S.empty
                 Just (e,_) -> gFreeVars e
    let used' = getUsedFns used S.empty used
    let (fundefs',deleted) =
            M.foldr (\fn (acc1,acc2) ->
                         let f = funName fn in
                           if f `S.member` used' || L.isPrefixOf "_" (fromVar f)
                           then (M.insert f fn acc1, acc2)
                           else (acc1, f:acc2))
                    (M.empty,[])
                    fundefs
    dbgTrace 3 ("Removed unused functions: " ++ show deleted) $
      pure (Prog ddefs fundefs' main)
  where
    getUsedFns :: S.Set Var -> S.Set Var -> S.Set Var -> S.Set Var
    getUsedFns todo inspected acc =
      if S.null todo
      then acc
      else
        let f = S.elemAt 0 todo
            todo' = S.deleteAt 0 todo
        in if f `S.member` inspected
           then getUsedFns todo' inspected acc
           else
             let FunDef{funArgs, funName,funBody} = fundefs M.! f
                 free = gFreeVars funBody `S.difference` S.fromList (funName : funArgs)
             in getUsedFns (free <> todo') (S.insert f inspected) (acc <> free)

simplifyL1 :: Prog1 -> PassM Prog1
simplifyL1 p0 = do
    p0' <- freshNames1 p0
    p1 <- markRecFns p0'
    p2 <- inlineFuns p1
    deadFunElim p2

--------------------------------------------------------------------------------

simplifyLocBinds :: Prog2 -> PassM Prog2
simplifyLocBinds (Prog ddefs fundefs mainExp) = do
    let fundefs' = M.map gofun fundefs
    let mainExp' = case mainExp of
                     Just (e,ty) -> Just (go0 M.empty M.empty e, ty)
                     Nothing     -> Nothing
    pure $ Prog ddefs fundefs' mainExp'

  where
    gofun f@FunDef{funBody} =
        let funBody' = go0 M.empty M.empty funBody
        in f { funBody = funBody' }

    -- partially evaluate location arithmetic
    go0 :: M.Map LocExp LocVar -> M.Map LocVar LocVar -> Exp2 -> Exp2
    go0 env1 env2 ex =
      case ex of
        AppE f locs args -> AppE f (map (substloc env2) locs) (map (go0 env1 env2) args)
        PrimAppE p args -> PrimAppE p (map (go0 env1 env2) args)
        LetE (v,locs,ty,rhs) bod -> LetE (v,locs,substLoc env2 ty,go0 env1 env2 rhs) (go0 env1 env2 bod)
        IfE a b c -> IfE (go0 env1 env2 a) (go0 env1 env2 b) (go0 env1 env2 c)
        MkProdE args -> MkProdE (map (go0 env1 env2) args)
        ProjE i bod -> ProjE i (go0 env1 env2 bod)
        CaseE scrt brs -> CaseE (go0 env1 env2 scrt) (map (\(a,b,c) -> (a,b,go0 env1 env2 c)) brs)
        DataConE loc dcon args -> DataConE (substloc env2 loc) dcon (map (go0 env1 env2) args)
        TimeIt e ty b -> TimeIt (go0 env1 env2 e) ty b
        WithArenaE v bod -> WithArenaE v (go0 env1 env2 bod)
        SpawnE f locs args -> SpawnE f (map (substloc env2) locs) (map (go0 env1 env2) args)
        Ext ext ->
          case ext of
            LetRegionE reg sz ty bod -> Ext (LetRegionE reg sz ty (go0 env1 env2 bod))
            LetParRegionE reg sz ty bod -> Ext (LetParRegionE reg sz ty (go0 env1 env2 bod))
            LetLocE loc rhs bod ->
              let rhs' = case rhs of
                           AfterConstantLE i loc2 -> AfterConstantLE i (substloc env2 loc2)
                           AfterVariableLE v loc2 b -> AfterVariableLE v (substloc env2 loc2) b
                           _ -> rhs
              in case M.lookup rhs' env1 of
                Nothing  -> Ext (LetLocE loc rhs' (go0 (M.insert rhs' loc env1) env2 bod))
                Just new -> go0 env1 (M.insert loc new env2) bod
            LetAvail vars bod -> Ext (LetAvail vars (go0 env1 env2 bod))
            _ -> Ext ext
        _ -> ex
      where
        substloc env loc = fromMaybe loc (M.lookup loc env)

--------------------------------------------------------------------------------

lateInlineTriv :: L4.Prog -> PassM L4.Prog
lateInlineTriv (L4.Prog sym_tbl fundefs mainExp) = do
    let fundefs' = map lateInlineTrivFn fundefs
        mainExp' = case mainExp of
                       Just (L4.PrintExp tl) -> Just (L4.PrintExp (lateInlineTrivExp M.empty tl))
                       Nothing -> Nothing
    return $ L4.Prog sym_tbl fundefs' mainExp'
  where
    lateInlineTrivFn :: L4.FunDecl -> L4.FunDecl
    lateInlineTrivFn f@L4.FunDecl{L4.funBody} =
        let bod' = lateInlineTrivExp M.empty funBody
        in f {L4.funBody = bod'}

    lateInlineTrivExp :: M.Map Var L4.Triv -> L4.Tail -> L4.Tail
    lateInlineTrivExp = go
      where
        gotriv env trv =
            case trv of
                L4.VarTriv v -> fromMaybe trv (M.lookup v env)
                L4.ProdTriv ls -> L4.ProdTriv (map (gotriv env) ls)
                L4.ProjTriv i t -> L4.ProjTriv i (gotriv env t)
                _ -> trv

        goalts env alts =
            case alts of
                L4.TagAlts ls -> L4.TagAlts $ map (second (go env)) ls
                L4.IntAlts ls -> L4.IntAlts $ map (second (go env)) ls
        go env tl =
              case tl of
                   L4.RetValsT trvs ->
                       L4.RetValsT (map (gotriv env) trvs)
                   L4.AssnValsT upd mb_bod ->
                       L4.AssnValsT upd (fmap (go env) mb_bod)
                   L4.LetCallT async binds rator rands bod ->
                       L4.LetCallT async binds rator (map (gotriv env) rands) (go env bod)
                   L4.LetPrimCallT binds prim rands bod ->
                       L4.LetPrimCallT binds prim (map (gotriv env) rands) (go env bod)
                   L4.LetTrivT (v,_ty,trv) bod ->
                       case trv of
                           L4.VarTriv w -> case M.lookup w env of
                                            Nothing -> go (M.insert v trv env) bod
                                            Just trv' -> go (M.insert v trv' env) bod
                           _ -> go (M.insert v (gotriv env trv) env) bod
                   L4.LetIfT binds (trv,tl1,tl2) bod ->
                       L4.LetIfT binds (gotriv env trv, go env tl1, go env tl2) (go env bod)
                   L4.LetUnpackT binds ptr bod ->
                       L4.LetUnpackT binds ptr (go env bod)
                   L4.LetAllocT lhs vals bod ->
                       L4.LetAllocT lhs (map (second (gotriv env)) vals) (go env bod)
                   L4.LetAvailT vars bod ->
                       L4.LetAvailT vars (go env bod)
                   L4.IfT tst thn els ->
                       L4.IfT (gotriv env tst) (go env thn) (go env els)
                   L4.ErrT str -> L4.ErrT str
                   L4.LetTimedT isiter binds timed bod ->
                       L4.LetTimedT isiter binds (go env timed) (go env bod)
                   L4.Switch lbl trv alts mb_tl ->
                       L4.Switch lbl trv (goalts env alts) (fmap (go env) mb_tl)
                   L4.TailCall var trvs ->
                       L4.TailCall var (map (gotriv env) trvs)
                   L4.Goto lbl -> L4.Goto lbl
                   L4.LetArenaT lhs bod ->
                       L4.LetArenaT lhs (go env bod)
