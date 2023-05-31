{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

-- | Put the program in A-normal form where only varrefs and literals are
-- allowed in operand position.
module Gibbon.Passes.Flatten
    ( flattenL0, flattenL1, flattenL2, flattenL3 ) where

import Control.Monad.State
import Text.PrettyPrint.GenericPretty
import Prelude hiding (exp)
import qualified Data.Map as M

import Gibbon.Common
import Gibbon.L1.Syntax
import Gibbon.L2.Syntax
import Gibbon.L3.Syntax
import qualified Gibbon.L0.Syntax as L0


-- | Flatten ensures that function operands are "trivial".
--
--   In the process, it also lifts lets out of case scrutinees, if
--   conditions, and tuple operands.
--
--   Note that it does not require tail expressions to be trivial.
--   For example, it allows AppE and PrimAppE in the body of a
--   let-expression.
flattenL1 :: Prog1 -> PassM Prog1
flattenL1 prg@(Prog defs funs main) = do
    main' <- case main of
               Just (e,ty) -> Just . (,ty) <$> gFlattenExp defs env20 e
               Nothing -> return Nothing
    funs' <- flattenFuns funs
    return $ Prog defs funs' main'
  where
    flattenFuns = mapM flattenFun
    flattenFun (FunDef nam narg (targ, ty) bod meta) = do
      let env2 = Env2 (M.fromList $ zip narg targ) (fEnv env20)
      bod' <- gFlattenExp defs env2 bod
      return $ FunDef nam narg (targ, ty) bod' meta

    env20 = progToEnv prg


flattenL2 :: Flattenable (E2Ext Var (UrTy LocVar)) => Prog2 -> PassM Prog2
flattenL2 prg@(Prog defs funs main) = do
    main' <-
      case main of
        Nothing -> return Nothing
        Just (ex,ty) -> fmap (Just . (,ty)) (gFlattenExp defs env20 ex)
    funs' <- flattenFuns funs
    return $ Prog defs funs' main'
  where
    flattenFuns = mapM flattenFun
    flattenFun (FunDef nam narg ty bod meta) = do
      let env2 = Env2 (M.fromList $ zip narg (arrIns ty)) (fEnv env20)
      bod' <- gFlattenExp defs env2 bod
      return $ FunDef nam narg ty bod' meta

    env20 = progToEnv prg


flattenL3 :: Prog3 -> PassM Prog3
flattenL3 prg@(Prog defs funs main) = do
    main' <-
      case main of
        Nothing -> return Nothing
        Just (ex,ty) -> fmap (Just . (,ty)) (gFlattenExp defs env20 ex)
    funs' <- flattenFuns funs
    return $ Prog defs funs' main'
  where
    flattenFuns = mapM flattenFun
    flattenFun (FunDef nam narg ty bod meta) = do
      let env2 = Env2 (M.fromList $ zip narg (fst ty)) (fEnv env20)
      bod' <- gFlattenExp defs env2 bod
      return $ FunDef nam narg ty bod' meta

    env20 = progToEnv prg


-- NOTE: / FIXME
-- If we would just include arrow types in the grammar from the start,
-- the the typeenv could contain function types too.  Data constructors could
-- go in there too.  Everything would be simpler. We would simply have to
-- use other means to remember that L1 programs are first order.

-- type Binds e = (Var,[LocOf e],TyOf e, e)


-- Constraints we need to write a generic Flatten.
type FlattenDeps e l d = (Show l, Out l, Show d, Out d,
                          Expression (e l d),
                          TyOf (e l d) ~ TyOf (PreExp e l d),
                          Typeable (PreExp e l d),
                          Flattenable (e l d))

instance FlattenDeps e l d => Flattenable (PreExp e l d) where

  gFlattenExp ddfs env ex = do (b,e') <- gFlattenGatherBinds ddfs env ex
                               return $ flatLets b e'
  gFlattenGatherBinds = exp


exp :: forall e l d. FlattenDeps e l d
    => DDefs (TyOf (PreExp e l d))
    -> Env2 (TyOf (PreExp e l d))
    -> PreExp e l d
    -> PassM ([Binds (PreExp e l d)], PreExp e l d)
exp ddfs env2 e0 =
  let triv :: String -> PreExp e l d -> PassM ([Binds (PreExp e l d)], PreExp e l d)
      triv m e = -- Force something to be trivial
        if isTrivial e
        then return ([],e)
        else do tmp <- gensym $ toVar $ "flt" ++ m
                let ty = gRecoverType ddfs env2 e
                (bnds,e') <- exp ddfs env2 e
                return ( bnds++[(tmp,[],ty,e')]
                       , VarE tmp)

      go :: PreExp e l d -> PassM ([Binds (PreExp e l d)], PreExp e l d)
      go = exp ddfs env2

      gols f ls m = do (bndss,ls') <- unzip <$> mapM (triv m) ls
                       return (concat bndss, f ls')

  in
  case e0 of
    Ext ext   -> do (_bnds,e) <- gFlattenGatherBinds ddfs env2 ext
                    return  ([], Ext e)

    LitE _    -> return ([],e0)
    CharE _   -> return ([],e0)
    FloatE{}  -> return ([],e0)
    VarE    _ -> return ([],e0)
    LitSymE _ -> return ([],e0)

    AppE f lvs ls     -> gols (AppE f lvs)  ls "AppE"
    PrimAppE p ls     -> gols (PrimAppE p)  ls "Prm"
    MkProdE ls        -> gols  MkProdE      ls "Prd"
    DataConE loc k ls -> gols (DataConE loc k) ls "Pkd"

    LetE (v1,lv1,t1, LetE (v2,lv2,t2,rhs2) rhs1) bod -> do
      (bnd, rhs) <- go (LetE (v2,lv2,t2,rhs2) $
                        LetE (v1,lv1,t1,rhs1) bod)
      return (bnd, rhs)

    LetE (v,locs,t,rhs) bod -> do (bnd1,rhs') <- go rhs
                                  (bnd2,bod') <- exp ddfs (extendVEnv v t env2) bod
                                  return (bnd1++[(v,locs,t,rhs')]++bnd2, bod')
    IfE a b c -> do (b1,a') <- triv "If" a
                    (b2,b') <- go b
                    (b3,c') <- go c
                    return (b1, IfE a' (flatLets b2 b') (flatLets b3 c'))
    -- This can happen anywhere, but doing it here prevents
    -- unneccessary bloat where we can ill afford it:
    ProjE ix (MkProdE ls) -> do
      -- dbgTrace 5 (" [flatten] Reducing project-of-tuple, index "++show ix++
      --             " expr:  "++take 80 (show ls)++"...")
      (bnd,rhs) <- go (ls !! ix)
      return (bnd, rhs)

    ProjE ix e -> do (b,e') <- triv "Prj" e
                     return (b, ProjE ix e')

    CaseE e ls -> do (b,e') <- triv "Cse" e
                     ls' <- forM ls $ \ (k,vrs,rhs) -> do
                              let tys = lookupDataCon ddfs k
                                  vrs' = map fst vrs
                                  env2' = extendsVEnv (M.fromList (zip vrs' tys)) env2
                              (b2,rhs') <- exp ddfs env2' rhs
                              return (k,vrs, flatLets b2 rhs')
                     return (b, CaseE e' ls')
    -- TimeIt is treated like a conditional.  Don't lift out of it:
    TimeIt e _t b -> do
      (bnd,e') <- go e
      return ([], TimeIt (flatLets bnd e') (gRecoverType ddfs env2 e) b)

    SpawnE f lvs ls -> gols (SpawnE f lvs)  ls "SpawnE"
    SyncE -> pure ([], SyncE)

    WithArenaE v e -> do
      (bnd, e') <- go e
      return ([], WithArenaE v (flatLets bnd e'))

    MapE _ _      -> error "FINISHLISTS"
    FoldE {}   -> error "FINISHLISTS"

-----------------------------------------------------------------------------------------

-- We have duplicate code here because exp depends on Typeable, and it cannot
-- be derived for L0. See the comment above L0.Syntax.recoverType for details.

flattenL0 :: L0.Prog0 -> PassM L0.Prog0
flattenL0 prg@(Prog defs funs main) = do
    main' <-
      case main of
        Nothing -> return Nothing
        Just (ex,ty) -> fmap (Just . (,ty)) (snd <$> flattenExp0 defs env20 ex)
    funs' <- flattenFuns funs
    return $ Prog defs funs' main'
  where
    flattenFuns = mapM flattenFun
    flattenFun (FunDef nam nargs ty bod meta) = do
      let env2 = Env2 (M.fromList $ zip nargs (L0.arrIns ty)) (fEnv env20)
      bod' <- snd <$> flattenExp0 defs env2 bod
      return $ FunDef nam nargs ty bod' meta
    env20 = progToEnv prg

flattenExp0 :: L0.DDefs0 -> Env2 L0.Ty0 -> L0.Exp0
            -> PassM ([Binds L0.Exp0], L0.Exp0)
flattenExp0 ddfs env2 e0 =
  let triv :: String -> L0.Exp0 -> PassM ([Binds L0.Exp0], L0.Exp0)
      triv m e = -- Force something to be trivial
        if isTrivial e
        then return ([],e)
        else do tmp <- gensym $ toVar $ "flt" ++ m
                let ty = L0.recoverType ddfs env2 e
                (bnds,e') <- flattenExp0 ddfs env2 e
                return ( bnds++[(tmp,[],ty,e')]
                       , VarE tmp)

      go :: L0.Exp0 -> PassM ([Binds L0.Exp0], L0.Exp0)
      go = flattenExp0 ddfs env2

      gols f ls m = do (bndss,ls') <- unzip <$> mapM (triv m) ls
                       dbgTraceIt (sdoc ls) (pure ())
                       return (concat bndss, f ls')

  in
  case e0 of
    LitE _    -> return ([],e0)
    CharE _   -> return ([],e0)
    FloatE{}  -> return ([],e0)
    VarE    _ -> return ([],e0)
    LitSymE _ -> return ([],e0)

    AppE f lvs ls     -> gols (AppE f lvs)  ls "AppE"
    PrimAppE p ls     -> gols (PrimAppE p)  ls "Prm"
    MkProdE ls        -> gols  MkProdE      ls "Prd"
    DataConE loc k ls -> gols (DataConE loc k) ls "Pkd"

    LetE (v1,lv1,t1, LetE (v2,lv2,t2,rhs2) rhs1) bod -> do
      (bnd, rhs) <- go (LetE (v2,lv2,t2,rhs2) $
                        LetE (v1,lv1,t1,rhs1) bod)
      return (bnd, rhs)

    LetE (v,locs,t,rhs) bod -> do (bnd1,rhs') <- go rhs
                                  (bnd2,bod') <- flattenExp0 ddfs (extendVEnv v t env2) bod
                                  return (bnd1++[(v,locs,t,rhs')]++bnd2, bod')
    IfE a b c -> do (b1,a') <- triv "If" a
                    (b2,b') <- go b
                    (b3,c') <- go c
                    return (b1, IfE a' (flatLets b2 b') (flatLets b3 c'))
    -- This can happen anywhere, but doing it here prevents
    -- unneccessary bloat where we can ill afford it:
    ProjE ix (MkProdE ls) -> do
      -- dbgTrace 5 (" [flatten] Reducing project-of-tuple, index "++show ix++
      --             " expr:  "++take 80 (show l)++"...")
      (bnd,rhs) <- go (ls !! ix)
      return (bnd, rhs)

    ProjE ix e -> do (b,e') <- triv "Prj" e
                     return (b, ProjE ix e')

    CaseE e ls -> do (b,e') <- triv "Cse" e
                     ls' <- forM ls $ \ (k,vrs,rhs) -> do
                              let tys = lookupDataCon ddfs k
                                  vrs' = map fst vrs
                                  env2' = extendsVEnv (M.fromList (zip vrs' tys)) env2
                              (b2,rhs') <- flattenExp0 ddfs env2' rhs
                              return (k,vrs, flatLets b2 rhs')
                     return (b, CaseE e' ls')
    -- TimeIt is treated like a conditional.  Don't lift out of it:
    TimeIt e _t b -> do
      (bnd,e') <- go e
      return ([], TimeIt (flatLets bnd e') (L0.recoverType ddfs env2 e) b)

    SpawnE f lvs ls -> gols (SpawnE f lvs)  ls "AppE"
    SyncE -> pure ([], SyncE)
    WithArenaE{}  -> error "flattenL0: WitnArenaE not handled."
    MapE _ _      -> error "FINISHLISTS"
    FoldE {}   -> error "FINISHLISTS"

    Ext ext ->
      case ext of
        L0.LambdaE args bod -> do
          (bnd1,bod') <- flattenExp0 ddfs (extendsVEnv (M.fromList args) env2) bod
          pure (bnd1, Ext $ L0.LambdaE args bod')
        L0.PolyAppE a b -> do
          (ba,a') <- go a
          (bb,b') <- go b
          pure ([], Ext $ L0.PolyAppE (flatLets ba a') (flatLets bb b'))
        L0.FunRefE{} -> pure ([], e0)
        L0.BenchE fn tyapps args b -> do
          (bnds, args') <- unzip <$> mapM go args
          pure (concat bnds, Ext $ L0.BenchE fn tyapps args' b)
        L0.ParE0 _ls -> error $ "flattenL0: ParE0 wasn't desugared: " ++ sdoc e0
        L0.PrintPacked ty arg -> do
          (bnds, arg') <- go arg
          pure (bnds, Ext $ L0.PrintPacked ty arg')
        L0.CopyPacked ty arg -> do
          (bnds, arg') <- go arg
          pure (bnds, Ext $ L0.CopyPacked ty arg')
        L0.TravPacked ty arg -> do
          (bnds, arg') <- go arg
          pure (bnds, Ext $ L0.TravPacked ty arg')
        L0.L p e -> do
          (bnd1,e') <- flattenExp0 ddfs env2 e
          pure (bnd1, Ext $ L0.L p e')
        L0.LinearExt{} -> error $ "flattenExp0: a linear types extension wasn't desugared: " ++ sdoc e0
