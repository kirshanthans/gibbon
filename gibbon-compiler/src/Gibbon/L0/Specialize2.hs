{-# LANGUAGE DeriveAnyClass #-}

{- L0 Specializer (part 2):
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Paulette worked on a specializer which lives in 'Gibbon.L0.Specialize'
and specializes functions on curried calls. Now we need a driver which
takes these different pieces, and puts them together in order to
transform a fully polymorphic L0 program, into a monomorphic L1 program.
This module is the first attempt to do that.

-}

module Gibbon.L0.Specialize2 where

import           Control.Monad.State
import           Data.Foldable ( foldlM )
import qualified Data.Map as M
import qualified Data.Set as S
import           GHC.Stack (HasCallStack)
import           GHC.List (zip3)
import           Text.PrettyPrint.GenericPretty

import           Gibbon.Common
import           Gibbon.Pretty
import           Gibbon.L0.Syntax
import           Gibbon.L0.Typecheck
import qualified Gibbon.L1.Syntax as L1

--------------------------------------------------------------------------------

{-

Transforming L0 to L1
~~~~~~~~~~~~~~~~~~~~~

(A) Monomorphization
(B) Lambda lifting (via specialization)
(C) Convert to L1, which should be pretty straightforward at this point.



Monomorphization
~~~~~~~~~~~~~~~~

Things that can be polymorphic, and therefore should be monormorphized:
- top-level fn calls
- lamda functions
- datacons

Here's a rough plan:

(0) Walk over all datatypes and functions to collect obligations for
    polymorphic types which have been fully applied to monomorphic types in the
    source program. For example, in a function 'f :: Int -> Maybe Int', we must
    replace 'Maybe Int' with an appropriate monomorphic alternative.


(1) Start with main: walk over it, and collect all monomorphization obligations:

        { [((fn_name, [tyapp]), newname)] , [((lam_name, [tyapp]), newname)] , [((tycon, [tyapp]), newname)] }

    i.e fn_name should be monomorphized at [tyapp], and it should be named newname.

    While collecting these obligations, just replace all polymorphic things with their
    corresponding new names.

(1.2) 'main' can transitively call a polymorphic function via a monomorphic one.
      To collect those obligations, we walk over all the monomorphic functions in
      the program as well.

(2) Start monormorphizing toplevel functions, and collect any new obligations
    that may be generated. Repeat (2) until there are no more obls.

(3) Create monomorphic versions of all datatypes.

(4) After we have all the monomorphic datatypes, we need to fix TYPEs in (Packed TYPE ..) to
    have the correct suffix. Actually, this could be done in 'collectMonoObls', but we do
    it in a separate pass for now.

(5) Delete all polymorphic fns and datatypes, which should all just be dead code now.

(6) Typecheck monomorphic L0 once more.

TODOs:

(*) Curried functions are not supported atm (not even by the typechecker):
    they're a bit tricky to get right as Gibbon functions can only accept 1 argument.
(*) Support minimal 'import's in 'Gibbon.HaskellFronted'.
(*) Anonymous lambdas


Lambda lifting
~~~~~~~~~~~~~~

Assume that the input program is monomorphic.

(a) Traverse all expressions in the program (main and functions), and
    float out all lambda definitions to the top-level.

(b) Collect all function references passed in as arguments to other functions.
    E.g.

        foo :: (A -> B) -> A -> B
        main = ... (foo fn1 thing1) ... (foo fn2 thing2) ...

     => [ ((foo, [fn1]), foo_1), ((foo, [fn2]), foo_2), ... ]


(c) (foo fn1) and (foo fn2) would now be separate top-level first order functions:

        foo_1 :: A -> B
        foo_1 thing = ... fn1 thing ...

        foo_2 :: A -> B
        foo_2 thing = ... fn2 thing ...

    Create these functions, drop the lambdas from it's type, arguments etc.

-}

l0ToL1 :: Prog0 -> PassM L1.Prog1
l0ToL1 p = do
  p0 <- bindLambdas p
  p0' <- tcProg p0

  p1 <- monomorphize p0'
  dbgTrace 5 ("\n\nMonomorphized:\n" ++ (pprender p1)) (pure ())

  p1'  <- closureConvert p1
  dbgTrace 5 ("\n\nClosure converion:\n" ++ (pprender p1')) (pure ())
  p1'' <- tcProg p1'
  dbgTrace 5 ("\n\nTypechecked:\n" ++ (pprender p1'')) (pure ())

  p2 <- specLambdas p1
  dbgTrace 5 ("\n\nSpecialized:\n" ++ (pprender p2)) (pure ())
  p3 <- elimParE0 p2
  dbgTrace 5 ("\n\nEliminateParE0:\n" ++ (pprender p3)) (pure ())
  pure $ toL1 p3


-- Just a mechanical transformation ..
toL1 :: Prog0 -> L1.Prog1
toL1 Prog{ddefs, fundefs, mainExp} =
   Prog (M.map toL1DDef ddefs) (M.map toL1FunDef fundefs) mainExp'
  where
    mainExp' = case mainExp of
                 Nothing -> Nothing
                 Just (e,ty) -> Just (toL1Exp e, toL1Ty ty)

    toL1DDef :: DDef0 -> L1.DDef1
    toL1DDef ddf@DDef{dataCons} =
      ddf { dataCons = map (\(dcon, btys) -> (dcon, map (\(a,b) -> (a, toL1Ty b)) btys)) dataCons }

    toL1FunDef :: FunDef0 -> L1.FunDef1
    toL1FunDef fn@FunDef{funTy, funBody} =
      fn { funTy = toL1TyS funTy
         , funBody = toL1Exp funBody }

    toL1Exp :: Exp0 -> L1.Exp1
    toL1Exp ex =
      case ex of
        VarE v    -> L1.VarE v
        LitE n    -> L1.LitE n
        FloatE n  -> L1.FloatE n
        LitSymE v -> L1.LitSymE v
        AppE f [] args   -> AppE f [] (map toL1Exp args)
        AppE _ (_:_) _   -> err1 (sdoc ex)
        PrimAppE pr args ->
          case pr of
            -- This is always going to have a function reference which
            -- we cannot eliminate.
            VSortP{} ->
              case args of
                [ls, Ext (FunRefE _ fp)] ->
                  PrimAppE (toL1Prim pr) [toL1Exp ls, VarE fp]
                [ls, Ext (L _ (Ext (FunRefE _ fp)))] ->
                  PrimAppE (toL1Prim pr) [toL1Exp ls, VarE fp]
                _ -> PrimAppE (toL1Prim pr)(map toL1Exp args)
            InPlaceVSortP{} ->
              case args of
                [ls, Ext (FunRefE _ fp)] ->
                  PrimAppE (toL1Prim pr) [toL1Exp ls, VarE fp]
                [ls, Ext (L _ (Ext (FunRefE _ fp)))] ->
                  PrimAppE (toL1Prim pr) [toL1Exp ls, VarE fp]
                _ -> PrimAppE (toL1Prim pr)(map toL1Exp args)
            _ -> PrimAppE (toL1Prim pr) (map toL1Exp args)
        LetE (v,[],ty,rhs) bod -> LetE (v,[], toL1Ty ty, toL1Exp rhs) (toL1Exp bod)
        LetE (_,(_:_),_,_) _ -> err1 (sdoc ex)
        IfE a b c  -> IfE (toL1Exp a) (toL1Exp b) (toL1Exp c)
        MkProdE ls -> MkProdE (map toL1Exp ls)
        ProjE i a  -> ProjE i (toL1Exp a)
        CaseE scrt brs -> CaseE (toL1Exp scrt) (map (\(a,b,c) -> (a,
                                                                  map (\(x,_) -> (x,())) b,
                                                                  toL1Exp c) )
                                                    brs)
        DataConE _ dcon ls -> DataConE () dcon (map toL1Exp ls)
        TimeIt e ty b    -> TimeIt (toL1Exp e) (toL1Ty ty) b
        SpawnE _ (_:_) _ -> err1 (sdoc ex)
        SpawnE f [] args -> SpawnE f [] (map toL1Exp args)
        SyncE            -> SyncE
        WithArenaE v e -> WithArenaE v (toL1Exp e)
        MapE{}  -> err1 (sdoc ex)
        FoldE{} -> err1 (sdoc ex)
        Ext ext ->
          case ext of
            LambdaE{}  -> err2 (sdoc ex)
            PolyAppE{} -> err2 (sdoc ex)
            FunRefE{}  -> err2 (sdoc ex)
            BenchE fn tyapps args b ->
              case tyapps of
                [] -> Ext $ L1.BenchE fn [] (map toL1Exp args) b
                _  -> error "toL1: Polymorphic 'bench' not supported yet."
            ParE0{} -> error "toL1: ParE0"
            -- Erase srclocs while going to L1
            L _ e   -> toL1Exp e

    toL1Prim :: Prim Ty0 -> Prim L1.Ty1
    toL1Prim = fmap toL1Ty

    toL1Ty :: Ty0 -> L1.Ty1
    toL1Ty ty =
      case ty of
        IntTy   -> L1.IntTy
        FloatTy -> L1.FloatTy
        SymTy0  -> L1.SymTy
        BoolTy  -> L1.BoolTy
        TyVar{} -> err1 (sdoc ty)
        MetaTv{} -> err1 (sdoc ty)
        ProdTy tys  -> L1.ProdTy $ map toL1Ty tys
        SymDictTy (Just v) a -> L1.SymDictTy (Just v) $ toL1Ty a
        SymDictTy Nothing  a -> L1.SymDictTy Nothing $ toL1Ty a
        ArrowTy{} -> err2 (sdoc ty)
        PackedTy tycon tyapps | tyapps == [] -> L1.PackedTy tycon ()
                              | otherwise    -> err1 (sdoc ty)
        ArenaTy -> L1.ArenaTy
        SymSetTy -> L1.SymSetTy
        SymHashTy -> L1.SymHashTy
        IntHashTy -> error "toL1Ty: IntHashTy not handled."
        VectorTy a  -> L1.VectorTy (toL1Ty a)

    toL1TyS :: ArrowTy Ty0 -> ArrowTy L1.Ty1
    toL1TyS t@(ForAll tyvars (ArrowTy as b))
      | tyvars == [] = (map toL1Ty as, toL1Ty b)
      | otherwise    = err1 (sdoc t)
    toL1TyS (ForAll _ t) = error $ "toL1: Not a function type: " ++ sdoc t

    err1 msg = error $ "toL1: Program was not fully monomorphized. Encountered: " ++ msg

    err2 msg = error $ "toL1: Could not lift all lambdas. Encountered: " ++ msg

--------------------------------------------------------------------------------

-- The monomorphization monad.
type MonoM a = StateT MonoState PassM a

data MonoState = MonoState
  { mono_funs_todo :: M.Map (Var, [Ty0]) Var
  , mono_funs_done :: M.Map (Var, [Ty0]) Var
  , mono_lams      :: M.Map (Var, [Ty0]) Var
  , mono_dcons     :: M.Map (TyCon, [Ty0]) Var -- suffix
  }
  deriving (Show, Read, Ord, Eq, Generic, Out)

emptyMonoState :: MonoState
emptyMonoState = MonoState
  { mono_funs_todo = M.empty, mono_funs_done = M.empty
  , mono_lams = M.empty, mono_dcons = M.empty }

extendFuns :: (Var,[Ty0]) -> Var -> MonoState -> MonoState
extendFuns k v mono_st@MonoState{mono_funs_todo} =
  mono_st { mono_funs_todo = M.insert k v mono_funs_todo }

extendLambdas :: (Var,[Ty0]) -> Var -> MonoState -> MonoState
extendLambdas k v mono_st@MonoState{mono_lams} =
  mono_st { mono_lams = M.insert k v mono_lams }

extendDatacons :: (TyCon,[Ty0]) -> Var -> MonoState -> MonoState
extendDatacons k v mono_st@MonoState{mono_dcons} =
  mono_st { mono_dcons = M.insert k v mono_dcons }

-- We need this wrapper because of the way these maps are defined.
--
-- getLambdaObls id { mono_lams = [ ((id,[IntTy]), id1), ((id,[BoolTy]), id2) ] }
--   = [ (id2, [IntTy]), (id2, [BoolTy]) ]
getLambdaObls :: Var -> MonoState -> (M.Map Var [Ty0])
getLambdaObls f MonoState{mono_lams} =
  M.fromList $ map (\((_,tys), w) -> (w, tys)) f_mono_st
  where
    f_mono_st = filter (\((v,_), _) -> v == f) (M.toList mono_lams)


--------------------------------------------------------------------------------

monomorphize :: Prog0 -> PassM Prog0
monomorphize p@Prog{ddefs,fundefs,mainExp} = do
  let env2 = Env2 M.empty (M.map funTy fundefs)

  let mono_m = do
        -- Step (0)
        (ddfs0 :: [DDef0]) <- mapM (monoOblsDDef ddefs) (M.elems ddefs)
        let ddefs' = M.fromList $ map (\a -> (tyName a,a)) ddfs0
        -- Step (1)
        mainExp' <-
          case mainExp of
            Nothing -> pure Nothing
            Just (e,ty) -> do
              mainExp'  <- collectMonoObls ddefs' env2 toplevel e
              mainExp'' <- monoLambdas mainExp'
              mono_st   <- get
              assertLambdasMonomorphized mono_st
              pure $ Just (mainExp'', ty)
        -- Step (1.2)
        let mono_funs = M.filter isMonoFun fundefs
        mono_funs' <-
          foldlM
            (\funs fn@FunDef{funArgs,funName,funBody,funTy} -> do
                  let env2' = extendsVEnv (M.fromList $ zip funArgs (inTys funTy)) env2
                  let (ForAll tyvars (ArrowTy as b)) = funTy
                  as' <- mapM (monoOblsTy ddefs) as
                  b'  <- monoOblsTy ddefs b
                  funBody'  <- collectMonoObls ddefs' env2' toplevel funBody
                  funBody'' <- monoLambdas funBody'
                  mono_st <- get
                  assertLambdasMonomorphized mono_st
                  let fn' = fn { funBody = funBody'', funTy = (ForAll tyvars (ArrowTy as' b'))}
                  pure $ M.insert funName fn' funs)
            mono_funs
            (M.elems mono_funs)
        let fundefs' = mono_funs' `M.union` fundefs
        -- Step (2)
        fundefs'' <- monoFunDefs fundefs'
        -- N.B. Important to fetch the state before we run 'monoDDefs' which
        -- clears everything in 'mono_dcons'.
        mono_st <- get
        -- Step (3)
        ddefs'' <- monoDDefs ddefs'
        let p3 = p { ddefs = ddefs'', fundefs = fundefs'', mainExp = mainExp' }
        -- Step (4)
        let p4 = updateTyCons mono_st p3
        pure p4

  (p4,_) <- runStateT mono_m emptyMonoState

  -- Step (5)
  let p5  = purgePolyDDefs p4
  let p5' = purgePolyFuns p5
  -- Step (6)
  p6 <- tcProg p5'
  pure p6
  where
    toplevel = M.keysSet fundefs

    -- Create monomorphic versions of all polymorphic functions.
    monoFunDefs :: FunDefs0 -> MonoM FunDefs0
    monoFunDefs fundefs1 = do
      mono_st <- get
      if M.null (mono_funs_todo mono_st)
      then pure fundefs1
      else do
        let (((fun_name, tyapps), new_fun_name):rst) = M.toList (mono_funs_todo mono_st)
            fn@FunDef{funArgs, funName, funBody} = fundefs # fun_name
            tyvars = tyVarsFromScheme (funTy fn)
        assertSameLength ("While monormorphizing the function: " ++ sdoc funName) tyvars tyapps
        let mp = M.fromList $ zip tyvars tyapps
            funTy' = ForAll [] (substTyVar mp (tyFromScheme (funTy fn)))
            funBody' = substTyVarExp mp funBody
            -- Move this obligation from todo to done.
            mono_st' = mono_st { mono_funs_done = M.insert (fun_name, tyapps) new_fun_name (mono_funs_done mono_st)
                               , mono_funs_todo = M.fromList rst }
        put mono_st'
        -- Collect any more obligations generated due to the monormorphization
        let env21 = Env2 (M.fromList $ zip funArgs (inTys funTy')) (M.map funTy fundefs1)
        funBody'' <- collectMonoObls ddefs env21 toplevel funBody'
        funBody''' <- monoLambdas funBody''
        let fn' = fn { funName = new_fun_name, funTy = funTy', funBody = funBody''' }
        monoFunDefs (M.insert new_fun_name fn' fundefs1)

    -- Create monomorphic versions of all polymorphic datatypes.
    monoDDefs :: DDefs0 -> MonoM DDefs0
    monoDDefs ddefs1 = do
      mono_st <- get
      if M.null (mono_dcons mono_st)
      then pure ddefs1
      else do
        let (((tycon, tyapps), suffix):rst) = M.toList (mono_dcons mono_st)
            ddf@DDef{tyName,tyArgs,dataCons} = lookupDDef ddefs tycon
        assertSameLength ("In the datacon: " ++ sdoc tyName) tyArgs tyapps
        let tyName' = varAppend tyName suffix
            dataCons' = map
                          (\(dcon,vtys) ->
                            let (vars,tys) = unzip vtys
                                sbst = M.fromList (zip tyArgs tyapps)
                                tys' = map (substTyVar sbst) tys
                                tys'' = map (updateTyConsTy ddefs1 mono_st) tys'
                                vtys' = zip vars tys''
                            in (dcon ++ fromVar suffix, vtys'))
                          dataCons
            ddefs1' = M.insert tyName' (ddf { tyName = tyName', tyArgs = [], dataCons = dataCons' })  ddefs1
            mono_st'  = mono_st { mono_dcons = M.fromList rst }
        put mono_st'
        monoDDefs ddefs1'

    -- See examples/T127. Bar is monomorphic, but uses a monomorphized-by-hand
    -- Foo. We must update Bar to use the correct Foo.
    monoOblsDDef :: DDefs0 -> DDef0 -> MonoM DDef0
    monoOblsDDef ddefs1 d@DDef{dataCons} = do
      dataCons' <- mapM (\(dcon, args) -> (dcon,) <$> mapM (\(a,ty) -> (a,) <$> monoOblsTy ddefs1 ty) args) dataCons
      pure $ d{ dataCons = dataCons' }


-- After 'monoLambdas' runs, (mono_lams MonoState) must be empty
assertLambdasMonomorphized :: (Monad m, HasCallStack) => MonoState -> m ()
assertLambdasMonomorphized MonoState{mono_lams} =
  if M.null mono_lams
  then pure ()
  else error $ "Expected 0 lambda monormorphization obligations. Got " ++ sdoc mono_lams

assertSameLength :: (Out a, Out b, Monad m, HasCallStack) => String -> [a] -> [b] -> m ()
assertSameLength msg as bs =
  if length as /= length bs
  then error $ "assertSameLength: Type applications " ++ sdoc bs ++ " incompatible with the type variables: " ++
               sdoc as ++ ".\n " ++ msg
  else pure ()


monoOblsTy :: DDefs0 -> Ty0 -> MonoM Ty0
monoOblsTy ddefs1 t = do
  case t of
    IntTy     -> pure t
    FloatTy   -> pure t
    SymTy0    -> pure t
    BoolTy    -> pure t
    TyVar{}   -> pure t
    MetaTv{}  -> pure t
    ProdTy ls -> ProdTy <$> mapM (monoOblsTy ddefs1) ls
    SymDictTy{}  -> pure t
    ArrowTy as b -> do
      as' <- mapM (monoOblsTy ddefs1) as
      b' <- monoOblsTy ddefs1 b
      pure $ ArrowTy as' b'
    PackedTy tycon tyapps ->
      case tyapps of
        [] -> pure t
        -- We're only looking for fully monomorphized datatypes here
        _  -> case tyVarsInTys tyapps of
                [] -> do
                  tyapps' <- mapM (monoOblsTy ddefs1) tyapps
                  mono_st <- get
                  case M.lookup (tycon, tyapps') (mono_dcons mono_st) of
                    Nothing -> do
                      let DDef{tyArgs} = lookupDDef ddefs1 tycon
                      assertSameLength ("In the type: " ++ sdoc t) tyArgs tyapps'
                      suffix <- lift $ gensym "_v"
                      let mono_st' = extendDatacons (tycon, tyapps') suffix mono_st
                          tycon' = tycon ++ (fromVar suffix)
                      put mono_st'
                      pure $ PackedTy tycon' []
                    Just suffix -> pure $ PackedTy (tycon ++ (fromVar suffix)) []
                _  -> pure t
    VectorTy{} -> pure t
    ArenaTy  -> pure t
    SymSetTy -> error "monoOblsTy: SymSetTy not handled."
    SymHashTy-> error "monoOblsTy: SymHashTy not handled."
    IntHashTy-> error "monoOblsTy: IntHashTy not handled."


-- | Collect monomorphization obligations.
collectMonoObls :: DDefs0 -> Env2 Ty0 -> S.Set Var -> Exp0 -> MonoM Exp0
collectMonoObls ddefs env2 toplevel ex =
  case ex of
    AppE f [] args -> do
      args' <- mapM (collectMonoObls ddefs env2 toplevel) args
      pure $ AppE f [] args'
    AppE f tyapps args -> do
      args'   <- mapM (collectMonoObls ddefs env2 toplevel) args
      tyapps' <- mapM (monoOblsTy ddefs) tyapps
      f' <- addFnObl f tyapps'
      pure $ AppE f' [] args'
    LetE (v, [], ty@ArrowTy{}, rhs) bod ->do
      let env2' = (extendVEnv v ty env2)
      case rhs of
        Ext (LambdaE{}) -> do
          rhs' <- go rhs
          bod' <- collectMonoObls ddefs env2' toplevel bod
          pure $ LetE (v,[],ty,rhs') bod'
        _ -> do
          -- Special case for lambda bindings passed in as function arguments:
          --
          -- 'v' is an ArrowTy, but not a lambda defn -- this let binding must
          -- be in a function body, and 'v' must be a lambda that's
          -- passed in as an argument. We don't want to monormorphize it here.
          -- It'll be handled when the the outer fn is processed.
          -- To ensure that (AppE v ...) stays the same, we add 'v' into
          -- mono_st s.t. it's new name would be same as it's old name.
          state (\st -> ((), extendLambdas (v,[]) v st))
          rhs' <- go rhs
          bod' <- collectMonoObls ddefs env2' toplevel bod
          pure $ LetE (v, [], ty, rhs') bod'

    LetE (v,[],ty,rhs) bod -> do
      let env2' = (extendVEnv v ty env2)
      rhs' <- go rhs
      bod' <- collectMonoObls ddefs env2' toplevel bod
      pure $ LetE (v,[],ty,rhs') bod'

    LetE (_, (_:_), _, _) _ -> error $ "collectMonoObls: Let not monomorphized: " ++ sdoc ex

    CaseE scrt brs -> do
      case recoverType ddefs env2 scrt of
        PackedTy tycon tyapps -> do
          mono_st <- get
          (suffix, mono_st'') <-
            case tyapps of
              -- It's a monomorphic datatype.
              [] -> pure ("", mono_st)
              _  -> do
                tyapps' <- mapM (monoOblsTy ddefs) tyapps
                case M.lookup (tycon, tyapps') (mono_dcons mono_st) of
                  Nothing -> do
                    let DDef{tyArgs} = lookupDDef ddefs tycon
                    assertSameLength ("In the expression: " ++ sdoc ex) tyArgs tyapps'
                    suffix <- lift $ gensym "_v"
                    let mono_st' = extendDatacons (tycon, tyapps') suffix mono_st
                    pure (suffix, mono_st')
                  Just suffix -> pure (suffix, mono_st)
          put mono_st''
          scrt' <- go scrt
          brs' <-
            foldlM
              (\acc (dcon,vtys,bod) -> do
                let env2' = extendsVEnv (M.fromList vtys) env2
                bod' <- collectMonoObls ddefs env2' toplevel bod
                pure $ acc ++ [(dcon ++ fromVar suffix,vtys,bod')])
              [] brs
          pure $ CaseE scrt' brs'

        ty -> error $ "collectMonoObls: Unexpected type for the scrutinee, " ++ sdoc ty ++
                      ". In the expression: " ++ sdoc ex

    DataConE (ProdTy tyapps) dcon args -> do
      args' <- mapM (collectMonoObls ddefs env2 toplevel) args
      case tyapps of
        -- It's a monomorphic datatype.
        [] -> pure $ DataConE (ProdTy []) dcon args'
        _  -> do
          mono_st <- get
          -- Collect datacon instances here.
          let tycon = getTyOfDataCon ddefs dcon
          tyapps' <- mapM (monoOblsTy ddefs) tyapps
          case M.lookup (tycon, tyapps') (mono_dcons mono_st) of
            Nothing -> do
              let DDef{tyArgs} = lookupDDef ddefs tycon
              assertSameLength ("In the expression: " ++ sdoc ex) tyArgs tyapps'
              suffix <- lift $ gensym "_v"
              let mono_st' = extendDatacons (tycon, tyapps) suffix mono_st
                  dcon' = dcon ++ (fromVar suffix)
              put mono_st'
              pure $ DataConE (ProdTy []) dcon' args'
            Just suffix -> do
              let dcon' = dcon ++ (fromVar suffix)
              pure $ DataConE (ProdTy []) dcon' args'

    DataConE{} -> error $ "collectMonoObls: DataConE expected ProdTy tyapps, got " ++ sdoc ex

    PrimAppE pr args -> do
      args' <- mapM (collectMonoObls ddefs env2 toplevel) args
      pure $ PrimAppE pr args'

    -- Straightforward recursion
    VarE{}    -> pure ex
    LitE{}    -> pure ex
    FloatE{}  -> pure ex
    LitSymE{} -> pure ex
    IfE a b c -> do
      a' <- go a
      b' <- go b
      c' <- go c
      pure $ IfE a' b' c'
    MkProdE args -> do
      args' <- mapM (collectMonoObls ddefs env2 toplevel) args
      pure $ MkProdE args'
    ProjE i e -> do
      e' <- go e
      pure $ ProjE i e'
    TimeIt e ty b -> do
      e' <- go e
      pure $ TimeIt e' ty b
    WithArenaE v e -> do
      e' <- go e
      pure $ WithArenaE v e'
    Ext ext ->
      case ext of
        LambdaE args bod -> do
          bod' <- collectMonoObls ddefs (extendsVEnv (M.fromList args) env2) toplevel bod
          pure $ Ext $ LambdaE args bod'
        PolyAppE{} -> error ("collectMonoObls: TODO, "++ sdoc ext)
        FunRefE tyapps f ->
          case tyapps of
            [] -> pure $ Ext $ FunRefE [] f
            _  -> do
              tyapps' <- mapM (monoOblsTy ddefs) tyapps
              f' <- addFnObl f tyapps'
              pure $ Ext $ FunRefE [] f'
        BenchE _fn tyapps _args _b ->
          case tyapps of
            [] -> pure ex
            _  -> error $ "collectMonoObls: Polymorphic bench not supported yet. In: " ++ sdoc ex
        ParE0 ls -> do
          ls' <- mapM (collectMonoObls ddefs env2 toplevel) ls
          pure $ Ext $ ParE0 ls'
        L p e -> do
          e' <- go e
          pure $ Ext $ L p e'
    SpawnE f [] args -> do
      args' <- mapM (collectMonoObls ddefs env2 toplevel) args
      pure $ SpawnE f [] args'
    SpawnE f tyapps args -> do
      args'   <- mapM (collectMonoObls ddefs env2 toplevel) args
      tyapps' <- mapM (monoOblsTy ddefs) tyapps
      f' <- addFnObl f tyapps'
      pure $ SpawnE f' [] args'
    SyncE    -> pure SyncE
    MapE{}  -> error $ "collectMonoObls: TODO: " ++ sdoc ex
    FoldE{} -> error $ "collectMonoObls: TODO: " ++ sdoc ex
  where
    go = collectMonoObls ddefs env2 toplevel

    -- 'fn' Could be either a lambda, or toplevel
    addFnObl :: Var -> [Ty0] -> MonoM Var
    addFnObl f tyapps = do
      mono_st <- get
      if f `S.member` toplevel
      then case (M.lookup (f,tyapps) (mono_funs_done mono_st), M.lookup (f,tyapps) (mono_funs_todo mono_st)) of
             (Nothing, Nothing) -> do
               new_name <- lift $ gensym f
               state (\st -> ((), extendFuns (f,tyapps) new_name st))
               pure new_name
             (Just fn_name, _) -> pure fn_name
             (_, Just fn_name) -> pure fn_name

      -- Why (f,[])? See "Special case for lambda bindings passed in as function arguments".
      else case (M.lookup (f,[]) (mono_lams mono_st), M.lookup (f,tyapps) (mono_lams mono_st)) of
             (Nothing, Nothing) -> do
               new_name <- lift $ gensym f
               state (\st -> ((),extendLambdas (f,tyapps) new_name st))
               pure new_name
             (_,Just lam_name) -> pure lam_name
             (Just lam_name,_) -> pure lam_name


-- | Create monomorphic versions of lambdas bound in this expression.
-- This does not float out the lambda definitions.
monoLambdas :: Exp0 -> MonoM Exp0
-- Assummption: lambdas only appear as RHS in a let.
monoLambdas ex =
  case ex of
    LetE (v,[],vty, rhs@(Ext (LambdaE args lam_bod))) bod -> do
      mono_st <- get
      let lam_mono_st = getLambdaObls v mono_st
      if M.null lam_mono_st
      -- This lambda is not polymorphic, don't monomorphize.
      then do
        bod' <- go bod
        lam_bod' <- monoLambdas lam_bod
        pure $ LetE (v, [], vty, (Ext (LambdaE args lam_bod'))) bod'
      -- Monomorphize and only bind those, drop the polymorphic defn.
      -- Also drop the obligation that we applied from MonoState.
      -- So after 'monoLambdas' is done, (mono_lams MonoState) should be [].
      else do
        -- new_lam_mono_st = old_lam_mono_st - applied_lam_mono_st
        let new_lam_mono_st = (mono_lams mono_st) `M.difference`
                              (M.fromList $ map (\(w,wtyapps) -> ((v,wtyapps), w)) (M.toList lam_mono_st))
            mono_st' = mono_st { mono_lams =  new_lam_mono_st }
        put mono_st'
        bod' <- monoLambdas bod
        monomorphized <- monoLamBinds (M.toList lam_mono_st) (vty, rhs)
        pure $ foldl (\acc bind -> LetE bind acc) bod' monomorphized

    LetE (_,(_:_),_,_) _ -> error $ "monoLambdas: Let not monomorphized: " ++ sdoc ex

    -- Straightforward recursion
    VarE{}    -> pure ex
    LitE{}    -> pure ex
    FloatE{}  -> pure ex
    LitSymE{} -> pure ex
    AppE f tyapps args ->
      case tyapps of
        [] -> do args' <- mapM monoLambdas args
                 pure $ AppE f [] args'
        _  -> error $ "monoLambdas: Expression probably not processed by collectMonoObls: " ++ sdoc ex
    PrimAppE pr args -> do args' <- mapM monoLambdas args
                           pure $ PrimAppE pr args'
    LetE (v,[],ty,rhs) bod -> do
      rhs' <- go rhs
      bod' <- monoLambdas bod
      pure $ LetE (v, [], ty, rhs') bod'
    IfE a b c  -> IfE <$> go a <*> go b <*> go c
    MkProdE ls -> MkProdE <$> mapM monoLambdas ls
    ProjE i a  -> (ProjE i) <$> go a
    CaseE scrt brs -> do
      scrt' <- go scrt
      brs'  <- mapM (\(a,b,c) -> (a,b,) <$> go c) brs
      pure $ CaseE scrt' brs'
    DataConE tyapp dcon args ->
      (DataConE tyapp dcon) <$> mapM monoLambdas args
    TimeIt e ty b  -> (\e' -> TimeIt e' ty b) <$> go e
    WithArenaE v e -> (\e' -> WithArenaE v e') <$> go e
    Ext (LambdaE{})  -> error $ "monoLambdas: Encountered a LambdaE outside a let binding. In\n" ++ sdoc ex
    Ext (PolyAppE{}) -> error $ "monoLambdas: TODO: " ++ sdoc ex
    Ext (FunRefE{})  -> pure ex
    Ext (BenchE{})   -> pure ex
    Ext (ParE0 ls)   -> Ext <$> ParE0 <$> mapM monoLambdas ls
    Ext (L p e)      -> Ext <$> (L p) <$> monoLambdas e
    SpawnE f tyapps args ->
      case tyapps of
        [] -> do args' <- mapM monoLambdas args
                 pure $ SpawnE f [] args'
        _  -> error $ "monoLambdas: Expression probably not processed by collectMonoObls: " ++ sdoc ex
    SyncE   -> pure SyncE
    MapE{}  -> error $ "monoLambdas: TODO: " ++ sdoc ex
    FoldE{} -> error $ "monoLambdas: TODO: " ++ sdoc ex
  where go = monoLambdas

        monoLamBinds :: [(Var,[Ty0])] -> (Ty0, Exp0) -> MonoM [(Var, [Ty0], Ty0, Exp0)]
        monoLamBinds [] _ = pure []
        monoLamBinds ((w, tyapps):rst) (ty,ex1) = do
          let tyvars = tyVarsInTy ty
          assertSameLength ("In the expression: " ++ sdoc ex1) tyvars tyapps
          let mp = M.fromList $ zip tyvars tyapps
              ty'  = substTyVar mp ty
              ex'  = substTyVarExp mp ex1
          (++ [(w, [], ty', ex')]) <$> monoLamBinds rst (ty,ex1)


-- | Remove all polymorphic functions and datatypes from a program. 'monoLambdas'
-- already gets rid of polymorphic mono_lams.
purgePolyFuns :: Prog0 -> Prog0
purgePolyFuns p@Prog{fundefs} =
  p { fundefs = M.filter isMonoFun fundefs }

isMonoFun :: FunDef0 -> Bool
isMonoFun FunDef{funTy} = (tyVarsFromScheme funTy) == []

purgePolyDDefs :: Prog0 -> Prog0
purgePolyDDefs p@Prog{ddefs} =
  p { ddefs = M.filter isMonoDDef ddefs }
  where
    isMonoDDef DDef{tyArgs} = tyArgs == []

-- See Step (4) in the big note. Lot of code duplication :(
updateTyCons :: MonoState -> Prog0 -> Prog0
updateTyCons mono_st p@Prog{ddefs, fundefs,mainExp}=
  let fundefs' = M.map fixFunDef fundefs
      mainExp' = case mainExp of
                   Nothing -> Nothing
                   Just (e,ty) -> Just (updateTyConsExp ddefs mono_st e, updateTyConsTy ddefs mono_st ty)
  in p { fundefs = fundefs', mainExp = mainExp' }
  where
    fixFunDef :: FunDef0 -> FunDef0
    fixFunDef fn@FunDef{funTy, funBody} =
      let funTy' = ForAll (tyVarsFromScheme funTy) (updateTyConsTy ddefs mono_st (tyFromScheme funTy))
          funBody' = updateTyConsExp ddefs mono_st funBody
      in fn { funTy = funTy', funBody = funBody' }

-- |
updateTyConsExp :: DDefs0 ->  MonoState -> Exp0 -> Exp0
updateTyConsExp ddefs mono_st ex =
  case ex of
    VarE{}    -> ex
    LitE{}    -> ex
    FloatE{}  -> ex
    LitSymE{} -> ex
    AppE f [] args    -> AppE f [] (map go args)
    AppE _ (_:_) _ -> error $ "updateTyConsExp: Call-site not monomorphized: " ++ sdoc ex
    PrimAppE pr args  -> PrimAppE pr (map go args)
    LetE (v,[],ty,rhs) bod -> LetE (v, [], updateTyConsTy ddefs mono_st ty, go rhs) (go bod)
    LetE (_,(_:_),_,_) _ -> error $ "updateTyConsExp: Let not monomorphized: " ++ sdoc ex
    IfE a b c  -> IfE (go a) (go b) (go c)
    MkProdE ls -> MkProdE (map go ls)
    ProjE i e  -> ProjE i (go e)
    CaseE scrt brs ->
      CaseE (go scrt) (map
                        (\(dcon,vtys,rhs) -> let (vars,tys) = unzip vtys
                                                 vtys' = zip vars $ map (updateTyConsTy ddefs mono_st) tys
                                             in (dcon, vtys', go rhs))
                        brs)
    DataConE (ProdTy tyapps) dcon args ->
      let tyapps' = map (updateTyConsTy ddefs mono_st) tyapps
          tycon   = getTyOfDataCon ddefs dcon
          dcon' = case M.lookup (tycon,tyapps') (mono_dcons mono_st) of
                    Nothing     -> dcon
                    Just suffix -> dcon ++ fromVar suffix
      -- Why [] ? The type arguments aren't required as the DDef is monomorphic.
      in DataConE (ProdTy []) dcon' (map go args)
    DataConE{} -> error $ "updateTyConsExp: DataConE expected ProdTy tyapps, got: " ++ sdoc ex
    TimeIt e ty b -> TimeIt (go e) (updateTyConsTy ddefs mono_st ty) b
    WithArenaE v e -> WithArenaE v (go e)
    SpawnE fn tyapps args -> SpawnE fn tyapps (map go args)
    SyncE   -> SyncE
    MapE{}  -> error $ "updateTyConsExp: TODO: " ++ sdoc ex
    FoldE{} -> error $ "updateTyConsExp: TODO: " ++ sdoc ex
    Ext (LambdaE args bod) -> Ext (LambdaE (map (\(v,ty) -> (v, updateTyConsTy ddefs mono_st ty)) args) (go bod))
    Ext (PolyAppE a b) -> Ext (PolyAppE (go a) (go b))
    Ext (FunRefE{})    -> ex
    Ext (BenchE{})     -> ex
    Ext (ParE0 ls)     -> Ext $ ParE0 $ map go ls
    Ext (L p e)        -> Ext $ L p (go e)
  where
    go = updateTyConsExp ddefs mono_st

-- | Update TyCons if an appropriate monomorphization obligation exists.
updateTyConsTy :: DDefs0 -> MonoState -> Ty0 -> Ty0
updateTyConsTy ddefs mono_st ty =
  case ty of
    IntTy   -> IntTy
    FloatTy -> FloatTy
    SymTy0  -> SymTy0
    BoolTy  -> BoolTy
    TyVar{} ->  error $ "updateTyConsTy: " ++ sdoc ty ++ " shouldn't be here."
    MetaTv{} -> error $ "updateTyConsTy: " ++ sdoc ty ++ " shouldn't be here."
    ProdTy tys  -> ProdTy (map go tys)
    SymDictTy v t -> SymDictTy v (go t)
    ArrowTy as b   -> ArrowTy (map go as) (go b)
    PackedTy t tys ->
      let tys' = map go tys
      in case M.lookup (t,tys') (mono_dcons mono_st) of
           Nothing     -> PackedTy t tys'
           -- Why [] ? The type arguments aren't required as the DDef is monomorphic.
           Just suffix -> PackedTy (t ++ fromVar suffix) []
    VectorTy t -> VectorTy (go t)
    ArenaTy -> ArenaTy
    SymSetTy -> SymSetTy
    SymHashTy -> SymHashTy
    IntHashTy -> IntHashTy
  where
    go = updateTyConsTy ddefs mono_st

--------------------------------------------------------------------------------

-- The specialization monad.
type SpecM a = StateT SpecState PassM a

type FunRef = Var

data SpecState = SpecState
  { sp_funs_todo :: M.Map (Var, [FunRef]) Var
  , sp_fundefs   :: FunDefs0 }
  deriving (Show, Eq, Generic, Out)

{-|

Specialization, only lambdas for now. E.g.

    foo :: (a -> b) -> a -> b
    foo f1 a = f1 a

    ... foo top1 x ...

becomes

    foo f1 a = ...

    foo2 :: a -> b
    foo2 a = top1 a

    ... foo2 x ...

-}
specLambdas :: Prog0 -> PassM Prog0
specLambdas prg@Prog{ddefs,fundefs,mainExp} = do
  let spec_m = do
        let env2 = progToEnv prg
        mainExp' <-
          case mainExp of
            Nothing -> pure Nothing
            Just (e, ty) -> do
              e' <- specLambdasExp ddefs fundefs env2 e
              pure $ Just (e', ty)
        -- Same reason as Step (1.2) in monomorphization.
        let fo_funs = M.filter isFOFun fundefs
        mapM_
          (\fn@FunDef{funName,funBody} -> do
                funBody' <- specLambdasExp ddefs fundefs env2 funBody
                low <- get
                let funs   = sp_fundefs low
                    fn'    = fn { funBody = funBody' }
                    funs'  = M.insert funName fn' funs
                    low' = low { sp_fundefs = funs' }
                put low'
                pure ())
          (M.elems fo_funs)
        fixpoint
        pure mainExp'

  (mainExp',low'') <- runStateT spec_m emptySpecState
  -- Get rid of all higher order functions.
  let fundefs' = purgeHO (sp_fundefs low'')
      prg' = prg { mainExp = mainExp', fundefs = fundefs' }
  -- Typecheck again.
  tcProg prg'
  where
    emptySpecState :: SpecState
    emptySpecState = SpecState M.empty fundefs

    -- Lower functions
    fixpoint :: SpecM ()
    fixpoint = do
      low <- get
      if M.null (sp_funs_todo low)
      then pure ()
      else do
        let fns = sp_fundefs low
            fn = fns # fn_name
            ((fn_name, refs), new_fn_name) = M.elemAt 0 (sp_funs_todo low)
        specLambdasFun ddefs fundefs (progToEnv prg) new_fn_name refs fn
        state (\st -> ((), st { sp_funs_todo = M.delete (fn_name, refs) (sp_funs_todo st) }))
        fixpoint

    purgeHO :: FunDefs0 -> FunDefs0
    purgeHO fns = M.filter isFOFun fns

    isFOFun :: FunDef0 -> Bool
    isFOFun FunDef{funTy} =
      let ForAll _ (ArrowTy arg_tys ret_ty) = funTy
      in all (null . arrowTysInTy) arg_tys &&
         arrowTysInTy ret_ty == []

-- Eliminate all functions passed in as arguments to this function.
specLambdasFun :: DDefs0 -> FunDefs0 -> Env2 Ty0 -> Var -> [FunRef] -> FunDef0 -> SpecM ()
specLambdasFun ddefs fundefs env2 new_fn_name refs fn@FunDef{funArgs, funTy} = do
  let
      -- lamda args
      funArgs'  = map fst $ filter (isFunTy . snd) $ zip funArgs (inTys funTy)
      specs     = fragileZip funArgs' refs
      -- non-lambda args
      funArgs'' = map fst $ filter (not . isFunTy . snd) $ zip funArgs (inTys funTy)
      fn' = fn { funName = new_fn_name
               , funBody = do_spec specs (funBody fn) }
  funBody' <- specLambdasExp ddefs fundefs env2 (funBody fn')
  let fn''  = fn' { funBody = funBody'
                  , funArgs = funArgs''
                  -- N.B. Only update the type after 'specExp' runs.
                  , funTy   = funTy' }
  state (\st -> ((), st { sp_fundefs = M.insert new_fn_name fn'' (sp_fundefs st) }))
  where
    ForAll tyvars (ArrowTy arg_tys ret_ty) = funTy

    -- TODO: What if the function returns another function ? Not handled yet.
    -- First order type
    funTy' = ForAll tyvars (ArrowTy (filter (not . isFunTy) arg_tys) ret_ty)

    do_spec :: [(Var,Var)] -> Exp0 -> Exp0
    do_spec lams e = foldr (uncurry subst') e lams

    subst' old new ex = gRename (M.singleton old new) ex

specLambdasExp :: DDefs0 -> FunDefs0 -> Env2 Ty0 -> Exp0 -> SpecM Exp0
specLambdasExp ddefs fundefs env2 ex =
  case ex of
    -- TODO, docs.
    AppE f [] args -> do
      args' <- mapM go args
      let args'' = dropFunRefs f args'
          refs   = foldr collectFunRefs [] args
      case refs of
        [] -> pure $ AppE f [] args'
        _  -> do
          low' <- get
          case M.lookup (f,refs) (sp_funs_todo low') of
            Nothing -> do
              f' <- lift $ gensym f
              let ForAll _ (ArrowTy as _) = lookupFEnv f env2
                  arrow_tys = concatMap arrowTysInTy as
              -- Check that the # of refs we collected actually matches the #
              -- of functions 'f' expects.
              assertSameLength ("While lowering the expression " ++ sdoc ex) refs arrow_tys
              -- We have a new lowering obligation.
              let low'' = low' { sp_funs_todo = M.insert (f,refs) f' (sp_funs_todo low') }
              put low''
              pure $ AppE f' [] args''
            Just f' -> pure $ AppE f' [] args''
    AppE _ (_:_) _ -> error $ "specLambdasExp: Call-site not monomorphized: " ++ sdoc ex

    -- Float out a lambda fun to the top-level.
    LetE (v, [], ty, (Ext (LambdaE args lam_bod))) bod -> do
      let arg_vars = map fst args
          captured_vars = gFreeVars lam_bod `S.difference` (S.fromList arg_vars) `S.difference` (M.keysSet fundefs)
      if not (S.null captured_vars)
      then error $ "specLambdasExp: LamdaE captures variables: "
                   ++ show captured_vars
                   ++ ". TODO: these can become additional arguments."
      else do
        lam_bod' <- go lam_bod
        let _fn_refs = collectFunRefs lam_bod []
            fn = FunDef { funName = v
                        , funArgs = arg_vars
                        , funTy   = ForAll [] ty
                        , funBody = lam_bod' }
            env2' = extendFEnv v (ForAll [] ty) env2
        state (\st -> ((), st { sp_fundefs = M.insert v fn (sp_fundefs st) }))
        specLambdasExp ddefs fundefs env2' bod

    LetE (v, [], ty, rhs) bod -> do
      let _fn_refs = collectFunRefs rhs []
          env2' = (extendVEnv v ty env2)
      rhs' <- go rhs
      bod' <- specLambdasExp ddefs fundefs env2' bod
      pure $ LetE (v, [], ty, rhs') bod'

    LetE (_, (_:_),_,_) _ -> error $ "specExp: Binding not monomorphized: " ++ sdoc ex

    -- Straightforward recursion
    VarE{}    -> pure ex
    LitE{}    -> pure ex
    FloatE{}  -> pure ex
    LitSymE{} -> pure ex
    PrimAppE pr args -> do
      args' <- mapM go args
      pure $ PrimAppE pr args'
    IfE a b c -> IfE <$> go a <*> go b <*> go c
    MkProdE ls -> MkProdE <$> mapM go ls
    ProjE i a -> (ProjE i) <$> go a
    CaseE scrt brs -> do
      scrt' <- go scrt
      brs' <- mapM
                (\(dcon,vtys,rhs) -> do
                  let env2' = extendsVEnv (M.fromList vtys) env2
                  (dcon,vtys,) <$> specLambdasExp ddefs fundefs env2' rhs)
                brs
      pure $ CaseE scrt' brs'
    DataConE tyapp dcon args -> (DataConE tyapp dcon) <$> mapM go args
    TimeIt e ty b -> do
       e' <- go e
       pure $ TimeIt e' ty b
    WithArenaE v e -> do
       e' <- specLambdasExp ddefs fundefs (extendVEnv v ArenaTy env2) e
       pure $ WithArenaE v e'
    SpawnE fn tyapps args -> do
      e' <- specLambdasExp ddefs fundefs env2 (AppE fn tyapps args)
      case e' of
        AppE fn' tyapps' args' -> pure $ SpawnE fn' tyapps' args'
        _ -> error "specLambdasExp: SpawnE"
    SyncE   -> pure SyncE
    MapE{}  -> error $ "specLambdasExp: TODO: " ++ sdoc ex
    FoldE{} -> error $ "specLambdasExp: TODO: " ++ sdoc ex
    Ext ext ->
      case ext of
        LambdaE{}  -> error $ "specLambdasExp: Should reach a LambdaE. It should be floated out by the Let case." ++ sdoc ex
        PolyAppE{} -> error $ "specLambdasExp: TODO: " ++ sdoc ex
        FunRefE{}  -> pure ex
        BenchE{}   -> pure ex
        ParE0 ls -> do
          let mk_fn :: Exp0 -> SpecM (Maybe FunDef0, [(Var, [Ty0], Ty0, (PreExp E0Ext Ty0 Ty0))], Exp0)
              mk_fn e0 = do
                let vars = S.toList $ gFreeVars e0
                args <- mapM (\v -> lift $ gensym v) vars
                let e0' = foldr (\(old,new) acc ->
                                  gSubst old (VarE new) acc)
                                e0
                                (zip vars args)
                -- let bind args = vars before call_a
                fnname <- lift $ gensym "fn"
                let binds  = map (\(v,w,ty) -> (v,[],ty,VarE w)) (zip3 args vars argtys)
                    retty  = recoverType ddefs env2 e0
                    argtys = map (\v -> lookupVEnv v env2) vars
                    fn = FunDef { funName = fnname
                                , funArgs = args
                                , funTy   = ForAll [] (ArrowTy argtys retty)
                                , funBody = e0'
                                }
                pure (Just fn, binds, AppE fnname [] (map VarE args))
          let mb_insert mb_fn mp = case mb_fn of
                                     Just fn -> M.insert (funName fn) fn mp
                                     Nothing -> mp
          (mb_fns, binds, calls) <- unzip3 <$> mapM (\a -> case a of
                                                  AppE{} -> pure (Nothing, [], a)
                                                  _ -> mk_fn a)
                                         ls
          state (\st -> ((), st { sp_fundefs = foldr mb_insert (sp_fundefs st) mb_fns }))
          pure $ mkLets (concat binds) (Ext $ ParE0 calls)
        L p e -> Ext <$> (L p) <$> go e
  where
    go = specLambdasExp ddefs fundefs env2

    _isFunRef e =
      case e of
        VarE v -> M.member v (fEnv env2)
        _ -> False

    -- fn_0 (fn_1, thing, fn_2) => fn_0 (thing)
    dropFunRefs :: Var -> [Exp0] -> [Exp0]
    dropFunRefs fn_name args =
      foldr (\(a,t) acc -> if isFunTy t then acc else a:acc) [] (zip args arg_tys)
      where
        ForAll _ (ArrowTy arg_tys _) = lookupFEnv fn_name env2

    collectFunRefs :: Exp0 -> [FunRef] -> [FunRef]
    collectFunRefs e acc =
      case e of
        VarE{}    -> acc
        LitE{}    -> acc
        FloatE{}  -> acc
        LitSymE{} -> acc
        AppE _ _ args   -> foldr collectFunRefs acc args
        PrimAppE _ args -> foldr collectFunRefs acc args
        LetE (_,_,_, rhs) bod -> foldr collectFunRefs acc [bod, rhs]
        IfE a b c  -> foldr collectFunRefs acc [c, b, a]
        MkProdE ls -> foldr collectFunRefs acc ls
        ProjE _ a  -> collectFunRefs a acc
        DataConE _ _ ls -> foldr collectFunRefs acc ls
        TimeIt a _ _   -> collectFunRefs a acc
        WithArenaE _ e1-> collectFunRefs e1 acc
        CaseE scrt brs -> foldr
                            (\(_,_,b) acc2 -> collectFunRefs b acc2)
                            (collectFunRefs scrt acc)
                            brs
        SpawnE _ _ args -> foldr collectFunRefs acc args
        SyncE     -> acc
        MapE{}  -> error $ "collectFunRefs: TODO: " ++ sdoc e
        FoldE{} -> error $ "collectFunRefs: TODO: " ++ sdoc e
        Ext ext ->
          case ext of
            LambdaE _ bod       -> collectFunRefs bod acc
            PolyAppE rator rand -> collectFunRefs rand (collectFunRefs rator acc)
            FunRefE _ f         -> f : acc
            BenchE{}            -> acc
            ParE0 ls            -> foldr collectFunRefs acc ls
            L _ e1              -> collectFunRefs e1 acc

--------------------------------------------------------------------------------

closureConvert :: Prog0 -> PassM Prog0
closureConvert p0 = do
    p1 <- bindLambdas p0
    dbgTrace 5 ("\n\nbindLambdas:\n" ++ (pprender p1)) (pure ())
    p1' <- tcProg p1
    dbgTrace 5 ("\n\ntypechecked:\n" ++ (pprender p1')) (pure ())
    p2 <- closeLambdas p1'
    dbgTrace 5 ("\n\ncloseLambdas:\n" ++ (pprender p2)) (pure ())
    p2' <- tcProg p2
    dbgTrace 5 ("\n\ntypechecked:\n" ++ (pprender p2')) (pure ())
    p3 <- hoistLambdas p2'
    dbgTrace 5 ("\n\nhoistLambdas:\n" ++ (pprender p3)) (pure ())
    p3' <- tcProg p3
    dbgTrace 5 ("\n\ntypechecked:\n" ++ (pprender p3')) (pure ())
    pure p3'

{-|

Let bind all anonymous lambdas.

    map (\x -> x + 1) [1,2,3]

becomes

   let lam_1 = (\x -> x + 1)
   in map lam_1 [1,2,3]

This is an intermediate step before the specializer turns the let bound
lambdas into top-level functions.

-}
bindLambdas :: Prog0 -> PassM Prog0
bindLambdas prg@Prog{fundefs,mainExp} = do
  mainExp' <- case mainExp of
                Nothing      -> pure Nothing
                Just (a, ty) -> Just <$> (,ty) <$> goExp a
  fundefs' <- mapM
                (\fn@FunDef{funBody} -> goExp funBody >>=
                                        \b' -> pure $ fn {funBody = b'})
                fundefs
  pure $ prg { fundefs = fundefs'
             , mainExp = mainExp' }
  where
    goExp :: Exp0 -> PassM Exp0
    goExp ex0 = gocap ex0
      where
      gocap ex = do (lets,ex') <- go ex
                    pure $ mkLets lets ex'
      go :: Exp0 -> PassM ([(Var,[Ty0],Ty0,Exp0)], Exp0)
      go e0 =
       case e0 of
        (Ext (LambdaE{})) -> do
          v  <- gensym "lam_"
          ty <- newMetaTy
          pure ([(v,[],ty,e0)], VarE v)
        (LetE (v,tyapps,t,rhs@(Ext LambdaE{})) bod) -> do
            (lts2, bod') <- go bod
            pure  (lts2, LetE (v,tyapps,t,rhs) bod')
        -- boilerplate
        (Ext (ParE0 ls)) -> do
          ls' <- mapM gocap ls
          pure ([], Ext $ ParE0 ls')
        (Ext PolyAppE{}) -> pure ([], e0)
        (Ext FunRefE{})  -> pure ([], e0)
        (Ext BenchE{})   -> pure ([], e0)
        (Ext (L p e1))     -> do
          (ls, e1') <- go e1
          pure (ls, Ext $ L p e1')
        (LitE _)      -> pure ([], e0)
        (FloatE{})    -> pure ([], e0)
        (LitSymE _)   -> pure ([], e0)
        (VarE _)      -> pure ([], e0)
        (PrimAppE{})  -> pure ([], e0)
        (AppE f tyapps args) -> do
          (ltss,args') <- unzip <$> mapM go args
          pure (concat ltss, AppE f tyapps args')
        (MapE _ _)    -> error "goExp.go: FINISHME MapE"
        (FoldE _ _ _) -> error "goExp.go: FINISHME FoldE"
        (LetE (v,tyapps,t,rhs) bod) -> do
           (lts1, rhs') <- go rhs
           bod' <- gocap bod
           pure  (lts1, LetE (v,tyapps,t,rhs') bod')
        (IfE e1 e2 e3) -> do
             (lts1, e1') <- go e1
             e2' <- gocap e2
             e3' <- gocap e3
             pure  (lts1, IfE e1' e2' e3')
        (ProjE i e)  -> do (lts,e') <- go e
                           pure  (lts, ProjE i e')
        (MkProdE es) -> do (ltss,es') <- unzip <$> mapM go es
                           pure (concat ltss, MkProdE es')
        (CaseE scrt ls) -> do (lts,scrt') <- go scrt
                              ls' <- mapM (\(a,b,c) -> (a,b,) <$> gocap c) ls
                              pure (lts, CaseE scrt' ls')
        (DataConE c loc es) -> do (ltss,es') <- unzip <$> mapM go es
                                  pure (concat ltss, DataConE c loc es')
        (SpawnE f tyapps args) -> do
          (ltss,args') <- unzip <$> mapM go args
          pure (concat ltss, SpawnE f tyapps args')
        (SyncE)    -> pure ([], SyncE)
        (WithArenaE v e) -> do
          e' <- (gocap e)
          pure ([], WithArenaE v e')
        (TimeIt e t b) -> do (lts,e') <- go e
                             pure (lts, TimeIt e' t b)

----------------------------------------

-- Closure conversion monad
type CcM a = StateT (DDefs0, FunDefs0) PassM a

envTyCon :: TyCon
envTyCon = "Env"

mkEnvTy :: Ty0
mkEnvTy = PackedTy envTyCon []

isEnvTy :: Ty0 -> Bool
isEnvTy = (==) mkEnvTy

emptyEnvCon :: DataCon
emptyEnvCon = "Mk_Env_0"

initEnvTyCon :: CcM ()
initEnvTyCon = do
    (ddefs,fundefs) <- get
    let ddf = DDef { tyName = toVar envTyCon
                   , tyArgs = []
                   , dataCons = [(emptyEnvCon,[])]
                   }
        ddefs' = M.insert (toVar envTyCon) ddf ddefs
    put (ddefs',fundefs)

allowedToCaptureTy :: Ty0 -> Bool
allowedToCaptureTy ty = isScalarTy0 ty || isValidListElemTy0 ty || isFunTy ty || isEnvTy ty

addConForTys :: [Ty0] -> CcM DataCon
addConForTys [] = pure emptyEnvCon
addConForTys tys
    | not (all allowedToCaptureTy tys) = error $ "Can only capture scalar types. Got: " ++ sdoc tys
    | otherwise = do
        (ddefs,fundefs) <- get
        let tys' = map
                    (\t -> case t of
                             ArrowTy in_tys ret_ty ->
                               ArrowTy (mkEnvTy : in_tys) ret_ty
                             _ -> t)
                    tys
            dcons = getConOrdering ddefs envTyCon
            matching_con = foldr
                             (\con acc ->
                                  let con_tys = lookupDataCon ddefs con in
                                  if con_tys == tys'
                                  then Just con
                                  else acc)
                             Nothing
                             dcons
        case matching_con of
            Just con -> pure con
            Nothing  -> do
                uniq <- lift $ newUniq
                let ddf = lookupDDef ddefs envTyCon
                    dcon_name = "Mk_" ++ envTyCon ++ "_" ++ show uniq
                    dcon = (dcon_name, map (False,) tys')
                    ddf' = ddf { dataCons = dcon : (dataCons ddf) }
                    ddefs' = M.insert (toVar envTyCon) ddf' ddefs
                put (ddefs',fundefs)
                pure dcon_name


-- | Maps a lambda (FunRef) to an environment.
type ClosEnv = M.Map Var Var

{-|

Lambdas are updated to read the values of free variables from an environment
which is given to them as an additonal argument. All call-sites of lambdas are
updated accordingly.

-}
closeLambdas :: Prog0 -> PassM Prog0
closeLambdas p0@(Prog ddefs0 fundefs0 _mainExp) =
    evalStateT (closeLambdas' p0) (ddefs0,fundefs0)

closeLambdas' :: Prog0 -> CcM Prog0
closeLambdas' (Prog _ddefs0 fundefs0 mainExp) = do
    initEnvTyCon
    mainExp' <- case mainExp of
                    Nothing -> pure Nothing
                    Just (a,ty) -> do
                        a' <- updCallSites M.empty init_env2 a
                        pure $ Just (a',ty)
    updaCallSitesFuns
    (ddefs'',fundefs'') <- get
    pure $ Prog ddefs'' fundefs'' mainExp'
  where
    init_env2 = Env2 M.empty (M.map funTy fundefs0)

    updaCallSitesFuns :: CcM ()
    updaCallSitesFuns = do
      (_ddefs,fundefs) <- get
      fundefs' <-
        foldlM
          (\fns fn -> do
                  let ForAll tvs (ArrowTy in_tys ret_ty) = funTy fn
                      args = funArgs fn
                  (args',in_tys',clos_env) <-
                      foldlM
                        (\(acc1,acc2,acc3) (arg,ty) ->
                             case ty of
                               ArrowTy in_tys1 ret_ty1 -> do
                                 env_arg <- lift $ gensym "env"
                                 let acc1' = acc1 ++ [arg,env_arg]
                                     acc2' = acc2 ++ [ArrowTy (mkEnvTy : in_tys1) ret_ty1, mkEnvTy]
                                     acc3' = M.insert arg env_arg acc3
                                 pure (acc1',acc2',acc3')
                               _ -> pure (acc1++[arg],acc2++[ty],acc3))
                        ([],[],M.empty)
                        (zip args in_tys)
                  let init_env2' = extendsVEnv (M.fromList (zip args in_tys)) init_env2
                  e' <- updCallSites clos_env init_env2' (funBody fn)
                  let fn' = fn { funArgs = args',
                                 funTy = ForAll tvs (ArrowTy in_tys' ret_ty),
                                 funBody = e' }
                      fns' = M.insert (funName fn) fn' fns
                  pure fns')
          M.empty
          fundefs
      (ddefs',_) <- get
      put (ddefs',fundefs')

    -- Update call sites to create and pass environments.
    updCallSites :: ClosEnv -> Env2 Ty0 -> Exp0 -> CcM Exp0
    updCallSites clos_env env2 ex = do
        (lets,ex') <- go clos_env env2 ex
        pure $ mkLets lets ex'

    getLamsEnv :: ClosEnv -> Exp0 -> Maybe Var
    getLamsEnv clos_env e =
      case e of
        Ext (FunRefE _tyapps f) -> M.lookup f clos_env
        _ -> Nothing

    go :: ClosEnv -> Env2 Ty0 -> Exp0 -> CcM ([(Var,[Ty0],Ty0,Exp0)], Exp0)
    go clos_env env2 e0 =
     case e0 of
      (Ext (LambdaE{})) -> do
        error $ "closeLambdas: Unbound lambda: " ++ sdoc e0

      (LetE
          (v,tyapps,lam_ty,_lam@(Ext (LambdaE args lam_bod)))
          let_bod) -> do
        (_,fundefs) <- get
        let arg_vars = map fst args
            captured_vars = (gFreeVars lam_bod `S.difference` (S.fromList arg_vars))
                            `S.difference` (M.keysSet fundefs)
            cvs = S.toList captured_vars
        let (cvs', env2') = foldr
                     (\w (acc,env2_acc) -> case M.lookup w clos_env of
                                    Nothing  -> (w : acc, env2_acc)
                                    Just env -> (w : env : acc, extendVEnv env mkEnvTy env2_acc))
                     ([], env2)
                     cvs
        let field_tys = map (\w -> lookupVEnv w env2') cvs'
        env_dcon <- addConForTys field_tys
        -- (1) update lambda function to fetch free variables from the environment
        env_arg <- lift $ gensym "env"
        (lts1, lam_bod'') <- go clos_env env2' lam_bod
        let args' = (env_arg, mkEnvTy) : args
            lam_bod' = CaseE (VarE env_arg) [(env_dcon,zip cvs' field_tys,lam_bod'')]
            lam' = LambdaE args' lam_bod'
            lam_ty' = case lam_ty of
                        ArrowTy arg_tys ret_ty -> ArrowTy (mkEnvTy : arg_tys) ret_ty
                        MetaTv{} -> lam_ty
                        _ -> lam_ty
            bind_lam = (v,tyapps,lam_ty',Ext lam')
        -- (2) create the environment that goes with this lambda
        env_name <- lift $ gensym "env_val"
        let env_rhs = DataConE (ProdTy []) env_dcon (map VarE cvs')
            bind_env = (env_name,[],mkEnvTy,env_rhs)
        let clos_env' = M.insert v env_name clos_env
        -- (3)
        (lts, let_bod') <- go clos_env' (extendVEnv v lam_ty' env2') let_bod
        pure $ (lts ++ lts1, mkLets [bind_lam,bind_env] let_bod')

      (AppE f tyapps args) -> do
        (_,fundefs) <- get
        (lts,args') <-
            foldlM
              (\(acc1,acc2) arg -> do
                   (lts1,arg') <- go clos_env env2 arg
                   let donothing = pure (acc1 ++ lts1, acc2 ++ [arg'])
                   case arg' of
                     Ext (FunRefE tyapps2 toplvl)  ->
                       case M.lookup toplvl fundefs of
                           Nothing ->
                             case getLamsEnv clos_env arg' of
                               Nothing  -> donothing
                               Just env -> pure (acc1 ++ lts1, acc2 ++ [arg', VarE env])
                           Just fn -> do
                             env_arg <- lift $ gensym "env"
                             let lam_args = (funArgs fn)
                             lam_tys <- mapM (\_ -> lift $ newMetaTy) lam_args
                             let lam_args' = env_arg : lam_args
                                 lam_tys' = mkEnvTy : lam_tys
                             lam_name <- lift $ gensym "lam_"
                             -- TODO: shouldn't use tyapps here;
                             let lam = LambdaE (zip lam_args' lam_tys') (AppE toplvl tyapps2 (map VarE lam_args))
                             lam_ty <- lift $ newMetaTy
                             let lam_bind = (lam_name,[],lam_ty,Ext lam)
                             case getLamsEnv clos_env arg' of
                               Nothing  -> pure (lam_bind : (acc1 ++ lts1), acc2 ++ [Ext (FunRefE tyapps2 lam_name), DataConE (ProdTy []) emptyEnvCon []])
                               Just env -> pure (lam_bind : (acc1 ++ lts1), acc2 ++ [Ext (FunRefE tyapps2 lam_name), VarE env])
                     _ -> donothing)
              ([],[])
              args
        let args'' = case M.lookup f clos_env of
                       Nothing  -> args'
                       Just env -> (VarE env) : args'
        pure (lts, AppE f tyapps args'')

      -- boilerplate
      (Ext (ParE0 ls)) -> do
        ls' <- mapM (updCallSites clos_env env2) ls
        pure ([], Ext $ ParE0 ls')
      (Ext PolyAppE{}) -> pure ([], e0)
      (Ext FunRefE{})  -> pure ([], e0)
      (Ext BenchE{})   -> pure ([], e0)
      (Ext (L p e1))     -> do
        (ls, e1') <- go clos_env env2 e1
        pure (ls, Ext $ L p e1')
      (LitE _)      -> pure ([], e0)
      (FloatE{})    -> pure ([], e0)
      (LitSymE _)   -> pure ([], e0)
      (VarE _)      -> pure ([], e0)
      (PrimAppE{})  -> pure ([], e0)
      (MapE _ _)    -> error "goExp.go: FINISHME MapE"
      (FoldE _ _ _) -> error "goExp.go: FINISHME FoldE"
      (LetE (v,tyapps,t,rhs) bod) -> do
          (lts1, rhs') <- go clos_env env2 rhs
          bod' <- updCallSites clos_env (extendVEnv v t env2) bod
          pure  (lts1, LetE (v,tyapps,t,rhs') bod')
      (IfE e1 e2 e3) -> do
           (lts1, e1') <- go clos_env env2 e1
           e2' <- updCallSites clos_env env2 e2
           e3' <- updCallSites clos_env env2 e3
           pure  (lts1, IfE e1' e2' e3')
      (ProjE i e)  -> do (lts,e') <- go clos_env env2 e
                         pure  (lts, ProjE i e')
      (MkProdE es) -> do (ltss,es') <- unzip <$> mapM (go clos_env env2) es
                         pure (concat ltss, MkProdE es')
      (CaseE scrt ls) -> do (lts,scrt') <- go clos_env env2 scrt
                            ls' <- mapM (\(a,b,c) -> (a,b,) <$> updCallSites clos_env env2 c) ls
                            pure (lts, CaseE scrt' ls')
      (DataConE c loc es) -> do (ltss,es') <- unzip <$> mapM (go clos_env env2) es
                                pure (concat ltss, DataConE c loc es')
      (SpawnE f tyapps args) -> do
        (ltss,args') <- unzip <$> mapM (go clos_env env2) args
        pure (concat ltss, SpawnE f tyapps args')
      (SyncE)    -> pure ([], SyncE)
      (WithArenaE v e) -> do
        e' <- updCallSites clos_env env2 e
        pure ([], WithArenaE v e')
      (TimeIt e t b) -> do (lts,e') <- go clos_env env2 e
                           pure (lts, TimeIt e' t b)

----------------------------------------

hoistLambdas :: Prog0 -> PassM Prog0
hoistLambdas prg@Prog{fundefs,mainExp} = do
  (lams1, mainExp') <-
    case mainExp of
        Nothing -> pure ([], Nothing)
        Just (a, ty) -> do
            (lams,a') <- go a
            pure (lams, Just (a',ty))
  (lams2,fundefs') <-
      foldlM
        (\(acc1,acc2) fn@FunDef{funName,funBody} -> do
              (lams,b') <- go funBody
              let fn' = fn {funBody = b'}
              pure (acc1 ++ lams, M.insert funName fn' acc2))
        (lams1,M.empty)
        fundefs
  let fundefs2 = map (\(v,_tyapps,ty,e) ->
                          case e of
                            Ext (LambdaE args bod) ->
                                let arg_vars = map fst args
                                    tyvars = tyVarsInTy ty
                                in ( v
                                   , FunDef { funName = v
                                            , funArgs = arg_vars
                                            , funTy = ForAll tyvars ty
                                            , funBody = bod })
                            _ -> error "not a lambda")
                     lams2
  pure $ prg { fundefs = fundefs' `M.union` (M.fromList fundefs2)
             , mainExp = mainExp' }
  where
      go :: Exp0 -> PassM ([(Var,[Ty0],Ty0,Exp0)], Exp0)
      go e0 =
       case e0 of
        (Ext (LambdaE{})) -> error $ "hoistLambdas: unbound lambda: " ++ sdoc e0
        (LetE (v,tyapps,t,lam@(Ext LambdaE{})) bod) -> do
            (lams, bod') <- go bod
            pure  ((v,tyapps,t,lam):lams, bod')
        -- boilerplate
        (Ext (ParE0 ls)) -> do
          (lams', ls') <- foldlM
                   (\(acc1,acc2) a -> do
                         (lams,a') <- go a
                         pure $ (acc1 ++ lams, acc2 ++ [a']))
                   ([],[])
                   ls
          pure (lams', Ext $ ParE0 ls')
        (Ext PolyAppE{}) -> pure ([], e0)
        (Ext FunRefE{})  -> pure ([], e0)
        (Ext BenchE{})   -> pure ([], e0)
        (Ext (L p e1))     -> do
          (lams, e1') <- go e1
          pure (lams, Ext $ L p e1')
        (LitE _)      -> pure ([], e0)
        (FloatE{})    -> pure ([], e0)
        (LitSymE _)   -> pure ([], e0)
        (VarE _)      -> pure ([], e0)
        (PrimAppE{})  -> pure ([], e0)
        (AppE f tyapps args) -> do
          (lamss,args') <- unzip <$> mapM go args
          pure (concat lamss, AppE f tyapps args')
        (MapE _ _)    -> error "goExp.go: FINISHME MapE"
        (FoldE _ _ _) -> error "goExp.go: FINISHME FoldE"
        (LetE (v,tyapps,t,rhs) bod) -> do
           (lams1, rhs') <- go rhs
           (lams2, bod') <- go bod
           pure  (lams1 ++ lams2, LetE (v,tyapps,t,rhs') bod')
        (IfE e1 e2 e3) -> do
             (lams1, e1') <- go e1
             (lams2, e2') <- go e2
             (lams3, e3') <- go e3
             pure  (lams1++lams2++lams3, IfE e1' e2' e3')
        (ProjE i e)  -> do (lams,e') <- go e
                           pure  (lams, ProjE i e')
        (MkProdE es) -> do (lamss,es') <- unzip <$> mapM go es
                           pure (concat lamss, MkProdE es')
        (CaseE scrt ls) -> do (lams1,scrt') <- go scrt
                              (lams2,ls') <-
                                  foldlM
                                    (\(acc, acc2) (a,b,c) -> do
                                          (lamsc,c') <- go c
                                          pure (acc ++ lamsc, acc2 ++ [(a,b,c')]))
                                    ([],[])
                                    ls
                              pure (lams1++lams2, CaseE scrt' ls')
        (DataConE c loc es) -> do (lamss,es') <- unzip <$> mapM go es
                                  pure (concat lamss, DataConE c loc es')
        (SpawnE f tyapps args) -> do
          (lamss,args') <- unzip <$> mapM go args
          pure (concat lamss, SpawnE f tyapps args')
        (SyncE)    -> pure ([], SyncE)
        (WithArenaE v e) -> do
          (lams,e') <- (go e)
          pure (lams, WithArenaE v e')
        (TimeIt e t b) -> do (lams,e') <- go e
                             pure (lams, TimeIt e' t b)

--------------------------------------------------------------------------------

-- | Convert parallel tuples to explicit spawn's and sync's.
elimParE0 :: Prog0 -> PassM Prog0
elimParE0 prg@Prog{fundefs,mainExp} = do
  fundefs' <- mapM (\fn@FunDef{funBody} -> go funBody >>= \b -> pure $ fn {funBody = b}) fundefs
  mainExp' <- case mainExp of
                Nothing     -> pure Nothing
                Just (e,ty) -> Just <$> (,ty) <$> go e
  pure $ prg { fundefs = fundefs', mainExp = mainExp' }
  where
    err1 msg = error $ "elimParE0: " ++ msg

    -- | Turn ParE0 into explicit spawn's and sync's
    go :: Exp0 -> PassM Exp0
    go ex =
      case ex of
        VarE{}    -> pure ex
        LitE{}    -> pure ex
        FloatE{}  -> pure ex
        LitSymE{} -> pure ex
        AppE f tyapps args-> AppE f tyapps <$> mapM go args
        PrimAppE pr args  -> do
          -- This is always going to have a function reference which
          -- we cannot eliminate.
          let args' =
                case pr of
                  VSortP{} ->
                    case args of
                      [ls, Ext (FunRefE _ fp)]             -> [ls, VarE fp]
                      [ls, Ext (L _ (Ext (FunRefE _ fp)))] -> [ls, VarE fp]
                      _ -> error $ "specExp: vsort" ++ sdoc ex
                  InPlaceVSortP{} ->
                    case args of
                      [ls, Ext (FunRefE _ fp)]             -> [ls, VarE fp]
                      [ls, Ext (L _ (Ext (FunRefE _ fp)))] -> [ls, VarE fp]
                      _ -> error $ "specExp: vsort" ++ sdoc ex
                  _ -> args
          args'' <- mapM go args'
          pure $ PrimAppE pr args''
        LetE (v,tyapps,ty@(ProdTy tys),(Ext (ParE0 ls))) bod -> do
          vs <- mapM (\_ -> gensym "par_") ls
          let ls' = foldr
                      (\(w,ty1,(AppE fn tyapps1 args)) acc ->
                         (w,[],ty1,(SpawnE fn tyapps1 args)) : acc)
                      []
                      (zip3 vs tys ls)
              ls'' = ls' ++ [("_", [], ProdTy [], SyncE), (v,tyapps,ty, MkProdE (map VarE vs))]
          bod' <- go bod
          pure $  mkLets ls'' bod'
        LetE (v,tyapps,ty,rhs) bod -> LetE <$> (v,tyapps,ty,) <$> go rhs <*> go bod
        IfE a b c  -> IfE <$> go a <*> go b <*> go c
        MkProdE ls -> MkProdE <$> mapM go ls
        ProjE i a  -> (ProjE i) <$> go a
        CaseE scrt brs -> CaseE <$> go scrt <*> (mapM (\(a,b,c) -> (a, b,) <$> go c) brs)
        DataConE a dcon ls -> DataConE a dcon <$> mapM go ls
        TimeIt e ty b    -> (\a -> TimeIt a ty b) <$> go e
        WithArenaE v e -> (WithArenaE v) <$> go e
        SpawnE fn tyapps args -> (SpawnE fn tyapps) <$> mapM go args
        SyncE   -> pure SyncE
        MapE{}  -> err1 (sdoc ex)
        FoldE{} -> err1 (sdoc ex)
        Ext ext ->
          case ext of
            LambdaE{}  -> err1 (sdoc ex)
            PolyAppE{} -> err1 (sdoc ex)
            FunRefE{}  -> err1 (sdoc ex)
            BenchE fn _tyapps args b -> (\a -> Ext $ BenchE fn [] a b) <$> mapM go args
            ParE0{} -> err1 "toL1: ParE0"
            L p e   -> Ext <$> (L p) <$> (go e)
