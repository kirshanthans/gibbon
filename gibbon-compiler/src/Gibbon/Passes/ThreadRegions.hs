module Gibbon.Passes.ThreadRegions where

import qualified Data.List as L
import Data.Maybe ( fromJust )
import qualified Data.Map as M

import Gibbon.Common
import Gibbon.DynFlags
import Gibbon.L2.Syntax as L2

--------------------------------------------------------------------------------

{-

Threading regions
~~~~~~~~~~~~~~~~~

Functions need end-of-regions cursors for various purposes. The output region
cursors are used for bounds checking (See [Infinite regions] in BoundsCheck).
The input region cursors are useful for garbage collection -- if there's an
indirection from R1 to R2 (input), we need to bump R2's refcount and therefore
need R2's cursor. This pass updates all call-sites to also pass region cursors.
They are prepended to the locations that AppE forms accept.
N.B. for output regions, we actually use end-of-chunk cursors, not
end-of-region cursors.

    AppE add1 [lin, lout] arg

becomes

    AppE add1 [regin, regout, lin, lout] arg


Moreover, functions must also return region cursors, at least for the output
regions. Consider this example:

    ...
    let (x, lout1) = AppE add1 [regin1, regout, lin1, lout] a1 in
    let (y, lout2) = AppE add1 [regin2, regout, lin2, lout1] a2 in
    ...

This is not correct. Because of bounds checking, the first call to add1 might
start using a new output chunk. And we shouldn't use regout in the second call
to add1 -- 'regout' is already full! So we have to thread these output regions,
just like we do the output locations.

    ...
    let (x, lout1, regout1) = AppE add1 [regin1, regout, lin1, lout] a1 in
    let (y, lout2, regout2) = AppE add1 [regin2, regout1, lin2, lout1] a2 in
    ...


-}


-- Maps a location to a region
type RegEnv = M.Map LocVar Var

-- Maps the LHS of a constructor to the region of it's last field. Because of
-- parallelism the last field of constructor may not be in the same region as
-- it's tag. The region of the last field represents the "finished writing
-- output here region", so that's the region that should be threaded.
type LastFieldRegEnv = M.Map Var Var

threadRegions :: L2.Prog2 -> PassM L2.Prog2
threadRegions Prog{ddefs,fundefs,mainExp} = do
  fds' <- mapM (threadRegionsFn ddefs fundefs) $ M.elems fundefs
  let fundefs' = M.fromList $ map (\f -> (funName f,f)) fds'
      env2 = Env2 M.empty (initFunEnv fundefs)
  mainExp' <- case mainExp of
                Nothing -> return Nothing
                Just (mn, ty) -> Just . (,ty) <$>
                  threadRegionsExp ddefs fundefs True M.empty env2 M.empty mn
  return $ Prog ddefs fundefs' mainExp'

threadRegionsFn :: DDefs Ty2 -> FunDefs2 -> L2.FunDef2 -> PassM L2.FunDef2
threadRegionsFn ddefs fundefs f@FunDef{funName,funArgs,funTy,funBody} = do
  let initRegEnv = M.fromList $ map (\(LRM lc r _) -> (lc, regionToVar r)) (locVars funTy)
      initTyEnv  = M.fromList $ zip funArgs (arrIns funTy)
      env2 = Env2 initTyEnv (initFunEnv fundefs)
  bod' <- threadRegionsExp ddefs fundefs False initRegEnv env2 M.empty funBody
  -- Boundschecking
  dflags <- getDynFlags
  let bod''
        | gopt Opt_BigInfiniteRegions dflags = bod'
        | isCopySansPtrsFunName funName = bod'
        | otherwise =
        let packed_outs = getPackedTys (arrOut funTy)
            locs_tycons = foldr
                            (\ty acc ->
                                  case ty of
                                    PackedTy t loc ->  M.insert loc t acc
                                    _ -> acc)
                            M.empty
                            packed_outs
        in foldr
              (\(LRM loc reg mode) acc ->
                if mode == Output
                then let rv = toEndV $ regionToVar reg
                         bc = boundsCheck ddefs (locs_tycons M.! loc)
                      in -- dbgTraceIt ("boundscheck" ++ sdoc ((locs_tycons M.! loc), bc)) $
                        LetE ("_",[],IntTy, Ext$ BoundsCheck bc rv loc) acc
                else acc)
              bod'
              (locVars funTy)
  return $ f {funBody = bod''}

threadRegionsExp :: DDefs Ty2 -> FunDefs2 -> Bool -> RegEnv -> Env2 Ty2
                 -> LastFieldRegEnv -> L2.Exp2 -> PassM L2.Exp2
threadRegionsExp ddefs fundefs isMain renv env2 lfenv ex =
  case ex of
    AppE f applocs args -> do
      let ty = gRecoverType ddefs env2 ex
          argtys = map (gRecoverType ddefs env2) args
          argtylocs = concatMap locsInTy argtys
          in_regs = foldr (\x acc -> case M.lookup x renv of
                                       Just r -> r:acc
                                       Nothing -> acc)
                    [] argtylocs
      -- If this function returns a Packed type, it'll have input and output
      -- locations and therefore, input and output regions.
      if hasPacked ty
      then do
        let out_tylocs = locsInTy ty
            out_regs   = map (renv #) out_tylocs
        let newapplocs = map toEndV in_regs ++ map toEndV out_regs  ++ applocs
        return $ AppE f newapplocs args
      -- Otherwise, only input regions.
      else do
        let newapplocs = map toEndV in_regs ++ applocs
        return $ AppE f newapplocs args

    LetE (v,locs,ty, AppE f applocs args) bod -> do
      let -- argtys = map (gRecoverType ddefs env2) args
          -- argtylocs = concatMap locsInTy argtys
          argtylocs = concatMap
                        (\arg ->
                             let argty = gRecoverType ddefs env2 arg in
                             case arg of
                               VarE w ->
                                 case argty of
                                   CursorTy -> [w]
                                   _ -> locsInTy argty
                               _ -> locsInTy argty)
                        args
          in_regs = foldr (\x acc -> case M.lookup x renv of
                                       Just r -> r:acc
                                       Nothing -> acc)
                    [] argtylocs
          -- Map EndOf locations to input regions
          renv' = M.union renv (M.fromList $ zip locs in_regs)

      -- Similar to the AppE case above, this one would have input and
      -- output regions.
      if hasPacked ty
      then do
        let tylocs = locsInTy ty
            out_regs   = map (renv #) tylocs
        out_regs' <- mapM gensym out_regs
        -- Update all locations to point to the fresh region
        let renv'' = foldr (\(lc,r,r') acc ->
                             M.insert lc r' $
                             M.map (\tyl -> if tyl == r then r' else tyl) acc)
                    renv'
                    (L.zip3 tylocs out_regs out_regs')
            newretlocs = map toEndV out_regs' ++ locs
            newapplocs = map toEndV in_regs ++ map toEndV out_regs  ++ applocs
        LetE (v, newretlocs, ty, AppE f newapplocs args) <$>
          threadRegionsExp ddefs fundefs isMain renv'' (extendVEnv v ty env2) lfenv bod
      -- Only input regions.
      else do
          let newapplocs = map toEndV in_regs ++ applocs
          LetE (v,locs,ty,  AppE f newapplocs args) <$>
            threadRegionsExp ddefs fundefs isMain renv' (extendVEnv v ty env2) lfenv bod

    LetE (v,locs,ty, SpawnE f applocs args) bod -> do
      let e' = LetE (v,locs,ty, AppE f applocs args) bod
      e'' <- threadRegionsExp ddefs fundefs isMain renv env2 lfenv e'
      pure $ changeAppToSpawn f args e''

    -- AUDITME: this causes all all DataConE's to return an additional cursor.
    LetE (v,locs,ty@(PackedTy _ loc), rhs@(DataConE _ _ args)) bod -> do
      let reg_of_tag = renv M.! loc
          lfenv' = case args of
                     [] -> lfenv
                     _  ->
                       let last_ty = gRecoverType ddefs env2 (last args) in
                       case last_ty of
                          PackedTy _ last_loc -> do
                            let reg_of_last_arg = renv M.! last_loc
                            if reg_of_tag /= reg_of_last_arg
                            then M.insert v reg_of_last_arg lfenv
                            else lfenv
                          _ -> lfenv
      LetE . (v,locs,ty,) <$> go rhs <*>
        threadRegionsExp ddefs fundefs isMain renv (extendVEnv v ty env2) lfenv' bod

    -- Sometimes, this expression can have RetE forms. We should collect and update
    -- the locs here appropriately.
    LetE (v,locs,ty, rhs@(TimeIt{})) bod -> do
       rhs' <- go rhs
       let retlocs = findRetLocs rhs'
           newlocs = retlocs ++ locs
       LetE (v, newlocs,ty, rhs') <$>
         threadRegionsExp ddefs fundefs isMain renv (extendVEnv v ty env2) lfenv bod

    LetE (v,locs,ty, rhs) bod ->
      LetE . (v,locs,ty,) <$> go rhs <*>
        threadRegionsExp ddefs fundefs isMain renv (extendVEnv v ty env2) lfenv bod

    WithArenaE v e ->
      WithArenaE v <$> threadRegionsExp ddefs fundefs isMain renv (extendVEnv v ArenaTy env2) lfenv e

    Ext ext ->
      case ext of
        AddFixed{} -> return ex
        LetLocE loc FreeLE bod ->
          Ext . LetLocE loc FreeLE <$>
            threadRegionsExp ddefs fundefs isMain renv env2 lfenv bod

        -- Update renv with a binding for loc
        LetLocE loc rhs bod -> do
          let reg = case rhs of
                      StartOfLE r  -> regionToVar r
                      InRegionLE r -> regionToVar r
                      AfterConstantLE _ lc   -> renv # lc
                      AfterVariableLE _ lc _ -> renv # lc
                      FromEndLE lc           -> renv # lc -- TODO: This needs to be fixed
          Ext . LetLocE loc rhs <$>
            threadRegionsExp ddefs fundefs isMain (M.insert loc reg renv) env2 lfenv bod

        RetE locs v -> do
          let ty = lookupVEnv v env2
          if not isMain && isPackedTy ty
          then
            case M.lookup v lfenv of
              Nothing -> do
                let tylocs  = locsInTy ty
                    regs    = map (renv #) tylocs
                    newlocs = map toEndV regs
                return $ Ext $ RetE (newlocs ++ locs) v
              Just r  -> return $ Ext $ RetE (toEndV r : locs) v
          else if hasPacked ty
          then do
            let tylocs  = locsInTy ty
                regs    = map (renv #) tylocs
                newlocs = map toEndV regs
            return $ Ext $ RetE (newlocs ++ locs) v
          else return $ Ext ext

        LetRegionE r sz ty bod -> Ext . LetRegionE r sz ty <$> go bod
        LetParRegionE r sz ty bod -> Ext . LetParRegionE r sz ty <$> go bod
        FromEndE{}    -> return ex
        BoundsCheck sz _bound cur -> return $ Ext $ BoundsCheck sz (toEndV (renv # cur)) cur
        IndirectionE{}   -> return ex
        GetCilkWorkerNum -> return ex
        LetAvail vs bod -> Ext . LetAvail vs <$> go bod

    -- Straightforward recursion

    VarE{}     -> return ex
    LitE{}     -> return ex
    CharE{}    -> return ex
    FloatE{}   -> return ex
    LitSymE{}  -> return ex
    PrimAppE{} -> return ex
    DataConE{} -> return ex
    ProjE i e  -> ProjE i <$> go e
    IfE a b c  -> IfE <$> go a <*> go b <*> go c
    MkProdE ls -> MkProdE <$> mapM go ls
    CaseE scrt mp -> do
      let (VarE v) = scrt
          PackedTy _ tyloc = lookupVEnv v env2
          reg = renv M.! tyloc
      CaseE scrt <$> mapM (docase reg renv env2 lfenv) mp
    TimeIt e ty b -> do
      e' <- go e
      return $ TimeIt e' ty b
    SpawnE{} -> error "threadRegionsExp: Unbound SpawnE"
    SyncE    -> pure ex
    MapE{}  -> error "threadRegionsExp: TODO MapE"
    FoldE{} -> error "threadRegionsExp: TODO FoldE"

  where
    go = threadRegionsExp ddefs fundefs isMain renv env2 lfenv

    docase reg renv1 env21 lfenv1 (dcon,vlocs,bod) = do
      -- Update the envs with bindings for pattern matched variables and locations.
      -- The locations point to the same region as the scrutinee.
      let (vars,locs) = unzip vlocs
          renv0  = if isIndirectionTag dcon || isRedirectionTag dcon
                   then foldr (`M.insert` reg) renv1 vars
                   else renv1
          renv1' = foldr (`M.insert` reg) renv0 locs
          env21' = extendPatternMatchEnv dcon ddefs vars locs env21
      (dcon,vlocs,) <$> threadRegionsExp ddefs fundefs isMain renv1' env21' lfenv1 bod


-- Inspect an AST and return locations in a RetE form.
findRetLocs :: Exp2 -> [LocVar]
findRetLocs e0 = go e0 []
  where
    go :: Exp2 -> [LocVar] -> [LocVar]
    go ex acc =
      case ex of
        VarE{}    -> acc
        LitE{}    -> acc
        CharE{}   -> acc
        FloatE{}  -> acc
        LitSymE{} -> acc
        AppE _ _ args   -> foldr go acc args
        PrimAppE _ args -> foldr go acc args
        LetE (_,_,_,rhs) bod -> foldr go acc [rhs,bod]
        IfE a b c  -> foldr go acc [a,b,c]
        MkProdE xs -> foldr go acc xs
        ProjE _ e  -> go e acc
        DataConE _ _ args -> foldr go acc args
        CaseE _ mp ->
          foldr (\(_,_,c) acc2 -> go c acc2) acc mp
        TimeIt e _ty _b  -> go e acc
        WithArenaE _v e -> go e acc
        SpawnE{} -> acc
        SyncE{}  -> acc
        Ext ext ->
          case ext of
            LetRegionE _ _ _ bod  -> go bod acc
            LetParRegionE _ _ _ bod  -> go bod acc
            LetLocE _ _ bod   -> go bod acc
            RetE locs _       -> locs ++ acc
            FromEndE{}        -> acc
            BoundsCheck{}     -> acc
            IndirectionE{}    -> acc
            AddFixed{}        -> acc
            GetCilkWorkerNum  -> acc
            LetAvail _ bod    -> go bod acc
        MapE{}  -> error "findRetLocs: TODO MapE"
        FoldE{}  -> error "findRetLocs: TODO FoldE"

----------------------------------------

-- Maximal sum of sizes of scalars before the first packed thing in the
-- constructors of this type. The assumption is that whatever writes
-- that packed value will do a bounds check again. Note that only AppE's
-- do boundschecking, DataConE's dont. We should fix this.
boundsCheck :: DDefs2 -> TyCon -> Int
boundsCheck ddefs tycon =
  let dcons = getConOrdering ddefs tycon
      spaceReqd = foldl (\(bytes, seen_packed) ty ->
                               if seen_packed
                               then ( bytes, seen_packed )
                               else if hasPacked ty
                               then ( bytes, True )
                               else ( bytes + fromJust (sizeOfTy ty), False ))
                        (0, False)
      tyss = map (lookupDataCon ddefs) dcons
      vals = map (fst . spaceReqd) tyss
      -- Add a byte for the tag.
      num_bytes = (1 + maximum vals)
  in if num_bytes < 32
     then 32
     else num_bytes
