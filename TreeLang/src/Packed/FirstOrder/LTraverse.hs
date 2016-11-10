{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

-- | An intermediate language with an effect system that captures traversals.

module Packed.FirstOrder.LTraverse
    ( Prog(..), Ty, FunDef(..), Effect(..), ArrowTy(..)
    , mapExprs, mapMExprs, progToEnv

    -- * Temporary backwards compatibility, plus rexports
    , Ty1(..), pattern SymTy
    , Exp(..)
      
    -- * Utilities for dealing with the extended types:
    , cursorTy, mkCursorTy, isCursorTy, cursorTyLoc, unknownCursor
    , hasRealPacked, isRealPacked, hasCursorTy
    , tyWithFreshLocs, stripTyLocs, getTyLocs
    , getFunTy, substTy, substEffs
    , cursorizeTy1, cursorizeTy2, mapPacked

    -- * Lattices of abstract locations:
    , Loc(..), LocVar
    , toWitnessVar, isWitnessVar, fromWitnessVar
    , toEndVar, isEndVar, fromEndVar
    , join, joins
    , allLocVars, argtyToLoc, mangle, subloc
    , LocEnv, extendLocEnv, getLocVar

    -- * Constraints
    , Constraint(..)
    )
    where

import Control.DeepSeq
import Packed.FirstOrder.Common hiding (FunDef)
import qualified Packed.FirstOrder.L1_Source as L1
import Packed.FirstOrder.L1_Source hiding (Ty, FunDef, Prog, mapExprs, progToEnv, fundefs)
import Data.List as L
import Data.Maybe
import Data.Set as S
import Data.Map as M
import Text.PrettyPrint.GenericPretty
    
--------------------------------------------------------------------------------

-- Unchanged from L1, or we could go A-normal:
-- data Exp =

-- | Abstract locations:
data Loc = Fixed Var -- ^ A rigid location, such as for an input or output field.
         | Fresh Var -- ^ Fresh location-variables as created by
                     -- calling functions that are polymorphic in
                     -- their output location.
         | TupLoc [Loc] -- ^ The locations for each part of a tuple.
         | Top    -- ^ Contradiction.  Locations couldn't unify.
         | Bottom -- ^ "don't know" or "don't care".  This is the
                  -- location for non-packed data.
  deriving (Read,Show,Eq,Ord, Generic, NFData)
instance Out Loc

-- Renaming conventions.  TODO: Use newtypes
--------------------------------------------

-- | Witness the location of a local variable.  Later these become
-- synonymous with the variables themselves.  But for some passes they
-- need to be differentiated.
toWitnessVar :: Var -> Var
-- Policy decision here:
-- witnessOf v = v
toWitnessVar = (witness_prefix ++)

witness_prefix :: String
-- witness_prefix = ""                  
witness_prefix = "witness_"

fromWitnessVar :: LocVar -> Maybe String
fromWitnessVar v | isWitnessVar v = Just (drop (length witness_prefix) v)
                 | otherwise = Nothing

isWitnessVar :: LocVar -> Bool
isWitnessVar = isPrefixOf witness_prefix
                            
---------

toEndVar :: LocVar -> LocVar
toEndVar v =
  if isEndVar v
  then error$ "toEndVar: cannot add an end marker to something already at end: "++show v
  else end_prefix ++ v

fromEndVar :: LocVar -> Maybe LocVar
fromEndVar v | isEndVar v = Just (drop (length end_prefix) v)
             | otherwise = Nothing

isEndVar :: LocVar -> Bool
isEndVar = isPrefixOf end_prefix

end_prefix :: String
end_prefix = "end_" -- Hacky way to encode end-of-region variables.

--------------------------------------------
             
    
-- | This should be a semi-join lattice.
join :: Loc -> Loc -> (Loc,[Constraint])
join Bottom y      = (y,[])
join x Bottom      = (x,[])
join Top _         = (Top,[])
join _   Top       = (Top,[])
join (Fresh v) (Fresh w) = (Fresh v, [Eql v w])
join (Fresh v) (Fixed w) = (Fixed w, [Eql v w])
join (Fixed v) (Fresh w) = (Fixed v, [Eql v w])
join (Fixed v) (Fixed w) | v == w    = (Fixed v, [])
                         | otherwise = (Top, [])
join (TupLoc l1) (TupLoc l2) =
    let (locs,cs) = unzip $ zipWith join l1 l2 in
    (TupLoc locs, concat cs)
join l1 l2 = error$ "join: locations have inconsistent shapes: "++show(doc (l1,l2))


joins :: [Loc]-> (Loc,[Constraint])
joins [] = (Bottom,[])
joins (a:b) = let (l,c) = joins b 
                  (l2,c2) = join a l
              in (l2,c++c2)

-- | We need equality for join and disequality for distinct fields'
--   and arguments' locations.
data Constraint = Eql Var Var
                | Neq Var Var
--                | EqlOffset Var (Int,Var)
  deriving (Read,Show,Eq,Ord, Generic, NFData)
instance Out Constraint

-- Our type for functions grows to include effects.
data ArrowTy t = ArrowTy { arrIn :: t, arrEffs:: (Set Effect), arrOut:: t }
  deriving (Read,Show,Eq,Ord, Generic, NFData)

data Effect = Traverse LocVar
  deriving (Read,Show,Eq,Ord, Generic, NFData)

-- instance Out Ty
instance Out t => Out (ArrowTy t)
instance Out Effect
instance Out a => Out (Set a) where
  docPrec n x = docPrec n (S.toList x)
  doc x = doc (S.toList x)
instance Out FunDef
instance Out Prog

-- | L1 Types extended with abstract Locations.
type Ty = L1.Ty1 LocVar

type NewFuns = M.Map Var FunDef
    
-- | Here we only change the types of FUNCTIONS:
data Prog = Prog { ddefs    :: DDefs L1.Ty
                 , fundefs  :: NewFuns
                 , mainExp  :: Maybe (L1.Exp, L1.Ty)
                 }
  deriving (Show, Read, Ord, Eq, Generic, NFData)

-- | Abstract some of the differences of top level program types, by
--   having a common way to extract an initial environment.  The
--   initial environment has types only for functions.
progToEnv :: Prog -> Env2 (Ty1 ())
progToEnv Prog{fundefs} = 
    Env2 M.empty
         (M.fromList [ (n,(fmap (\_->()) a, fmap (\_->()) b))
                     | FunDef n (ArrowTy a _ b) _ _ <- M.elems fundefs ])

           
-- | A function definition with the function's effects.
data FunDef = FunDef { funname :: Var
                     , funty   :: (ArrowTy Ty)
                     , funarg   :: Var
                     , funbod  :: L1.Exp }
  deriving (Show, Read, Ord, Eq, Generic, NFData)
--------------------------------------------------------------------------------

-- | Retrieve the type of a function:
getFunTy :: NewFuns -> Var -> ArrowTy Ty
getFunTy mp f = case M.lookup f mp of
                  Nothing -> error $ "getFunTy: function was not bound: "++show f
                  Just (FunDef{funty}) -> funty

                                    
-- | Retrieve all LocVars mentioned in a type
getTyLocs :: Ty -> Set LocVar
getTyLocs t =
    case t of
      IntTy  -> S.empty
      SymTy  -> S.empty
      BoolTy -> S.empty
      ProdTy ls -> S.unions (L.map getTyLocs ls)
      PackedTy _ lv -> S.singleton lv
      -- This is a tricky case:
      SymDictTy elt -> getTyLocs elt
      
                  
-- | Annotate a naked type with fresh location variables.
tyWithFreshLocs :: L1.Ty -> SyM Ty
tyWithFreshLocs t =
  case t of
    L1.Packed k -> PackedTy k <$> genLetter                   
    L1.IntTy    -> return IntTy
    L1.SymTy    -> return SymTy
    L1.BoolTy   -> return BoolTy
    L1.ProdTy l -> ProdTy <$> mapM tyWithFreshLocs l
    L1.SymDictTy v -> SymDictTy <$> tyWithFreshLocs v

-- | Remove the extra location annotations.
stripTyLocs :: Ty -> L1.Ty
stripTyLocs = fmap (const ())
  -- case t of
  --   PackedTy k _  -> L1.PackedTy k ()
  --   IntTy        -> L1.IntTy
  --   SymTy        -> L1.SymTy
  --   BoolTy       -> L1.BoolTy
  --   ProdTy l     -> L1.ProdTy    $ L.map stripTyLocs l
  --   SymDictTy v  -> L1.SymDictTy $ stripTyLocs v


-- | Apply a variable substitution to a type.
substTy :: Map LocVar LocVar -> Ty -> Ty
substTy mp t = go t
  where
    go t = 
     case t of
      IntTy  -> IntTy
      SymTy  -> SymTy
      BoolTy -> BoolTy
      SymDictTy te -> SymDictTy (go te)
      ProdTy    ts -> ProdTy    (L.map go ts)
      PackedTy k l
          | isEndVar l ->
              case M.lookup (fromJust $ fromEndVar l) mp of
                Just v  -> PackedTy k (toEndVar v)
                Nothing -> PackedTy k l
          | otherwise ->
              case M.lookup l mp of
                Just v  -> PackedTy k v
                Nothing -> PackedTy k l
                -- errorWithStackTrace $ "substTy: failed to find "++show l++
                --   "\n  in map: "++show mp++", when processing type "++show t

-- | Apply a substitution to an effect set.                                   
substEffs :: Map LocVar LocVar -> Set Effect -> Set Effect
substEffs mp ef =
    dbgTrace 5 ("\n  Substituting in effects "++show(mp,ef)) $ 
    S.map (\(Traverse v) ->
               case M.lookup v mp of
                 Just v2 -> Traverse v2
                 Nothing -> Traverse v) ef

-- | Collect all the locations mentioned in a type.
allLocVars :: Ty -> [LocVar]
-- TODO: could just be a fold
allLocVars t =
    case t of
      SymTy     -> []
      BoolTy    -> []
      IntTy     -> []
      PackedTy _ v -> [v]
      ProdTy ls  -> L.concatMap allLocVars ls
      SymDictTy elt -> allLocVars elt    
               

-- Cursor types encoded into the current language
--------------------------------------------------------------------------------

-- | A placeholder for a cursor witness that we don't know.
unknownCursor :: Var
unknownCursor = "UNKNOWN_CURSOR_VAL" 

-- Use a hack rather than extending the IR at this point:
cursorTy :: Ty
cursorTy = PackedTy "CURSOR_TY" ""

mkCursorTy :: a -> Ty1 a
mkCursorTy = PackedTy "CURSOR_TY" 

isCursorTy :: Ty1 a -> Bool
isCursorTy (PackedTy "CURSOR_TY" _) = True
isCursorTy _ = False

cursorTyLoc :: Show a => Ty1 a -> a
cursorTyLoc (PackedTy "CURSOR_TY" l) = l
cursorTyLoc t = error $ "cursorTyLoc: should only be called on a cursor type, not "++show t

-- | We need to ammend this function to NOT consider cursors as "packed".
-- TODO: switch to some other form of extension on the original Ty data structure!
hasRealPacked :: Ty1 a -> Bool
hasRealPacked t =
    case t of
      PackedTy{} -> not $ isCursorTy t
      ProdTy ls -> any hasRealPacked ls
      SymTy     -> False
      BoolTy    -> False
      IntTy     -> False
      SymDictTy t -> hasRealPacked t

hasCursorTy :: Ty1 a -> Bool
hasCursorTy t =
    case t of
      PackedTy{} -> isCursorTy t
      ProdTy ls -> any hasCursorTy ls
      SymTy     -> False
      BoolTy    -> False
      IntTy     -> False
      SymDictTy t -> hasCursorTy t

                     
isRealPacked :: Ty1 a -> Bool                                         
isRealPacked t@PackedTy{} = not (isCursorTy t)
isRealPacked _ = False

-- Cursorizing types:                 
--------------------------------------------------------------------------------                 

-- Cursorizing types.                   
--------------------------------------------------------------------------------
-- This happens in two stages, corresponding to the passes RouteEnds
-- and CursorDirect.

-- | Step 1/2: add additional outputs corresponding to
-- end-of-input-value witnesses.  Return the new type and the added
-- outputs.
cursorizeTy1 :: ArrowTy Ty -> (ArrowTy Ty, [LocVar])
cursorizeTy1 (ArrowTy inT ef ouT) = (newArr, newOut)
 where
  newArr = ArrowTy inT ef newOutTy
  newOutTy = prependArgs (L.map mkCursorTy newOut)
                         ouT
  -- Every _traversed_ packed input means a POTENTIAL output (new
  -- return value for the cursor's final value).
  newOut   = [ toEndVar v  -- This determines the ORDER of added inputs.
             | Traverse v <- S.toList ef ] -- ^ Because we traverse all outputs,
                                           -- this effect set  is just what we need.
             
-- | Step 2/2: finalize the conversion by:
--
--  (1) First, adding additional input arguments for the destination
--      cursors to which outputs are written.
--  (2) Packed types in the output then become end-cursors for those
--      same destinations.
--  (3) Packed types in the input likewise become (read-only) cursors.
--  (4) Finally, it REMOVES effect signatures, which are no longer needed.
-- 
--  RETURNS: the new type as well as the extra params added to the
--  input type.
cursorizeTy2 :: ArrowTy Ty -> (ArrowTy Ty, [LocVar])
cursorizeTy2 (ArrowTy inT ef ouT) = (newArr, newIn)
 where
  newArr  = ArrowTy newInTy S.empty newOutTy
  newInTy  = prependArgs (L.map mkCursorTy newIn)
                         (mapPacked (\_ l -> mkCursorTy l) inT)
  -- Let's turn output values into updated-output-cursors:
  newOutTy = mapPacked (\_ l -> mkCursorTy (toEndVar l)) ouT
  newIn    = allLocVars ouT -- These stay in their original order (preorder)

-- Injected cursor args go first in input and output:
prependArgs :: [Ty] -> Ty -> Ty
prependArgs [] t = t
prependArgs ls t = ProdTy $ ls ++ [t]

             

mapPacked :: (Var -> LocVar -> Ty) -> Ty -> Ty
mapPacked fn t =
  case t of
    IntTy  -> IntTy
    BoolTy -> BoolTy
    SymTy  -> SymTy
    (ProdTy x)    -> ProdTy $ L.map (mapPacked fn) x
    (SymDictTy x) -> SymDictTy $ mapPacked fn x
    PackedTy k l  -> fn k l

             
--------------------------------------------------------------------------------
                     
-- | Map every lexical variable in scope to an abstract location.
--   This is useful for compiler passes that need to track abstract
--   locations of program terms.
type LocEnv = M.Map Var Loc

-- | Convert the type of a function argument to an abstract location
-- for that function argument.
argtyToLoc :: Var -> Ty -> Loc
argtyToLoc v ty =
 case ty of   
  PackedTy{}
    | isCursorTy ty -> Fixed $ cursorTyLoc ty
    | otherwise -> Fixed v
    -- ^ Here we set the type based on the variable binding name, not the
    -- quantified loc variable in the type signature.
  (ProdTy ls)   -> TupLoc [argtyToLoc (subloc v i) t | (t,i) <- zip ls [1..]]
   -- ^ Here we generate fixed locations that are *subparts* of the function argument.
  SymTy         -> Bottom
  IntTy         -> Bottom
  BoolTy        -> Bottom
  SymDictTy _t  -> -- ^ This may contain packed objects, but it is not contiguous.
    Fixed v
    -- if hasPacked t then Top else Bottom

                             
-- A bit of name mangling when promoting lexical variables to location vars
---------------------------------------------------------------------------
-- | First, lift program variables so they don't interfere with ones
-- we introduce.  Also, remove internal underscores.
mangle :: Var -> Var
mangle v = v
-- mangle v = "_" ++ L.filter (/='_') v

-- | Refer to a portion of the data associated with a var.
subloc :: Var -> Int -> Var
subloc v n = v ++"_"++show n

-- Strip off any subloc modifiers
-- root :: Var -> Var
------------------------------------------------------------

-- | Take a location which is expected to be a single variable, and
-- retrieve that variable.
getLocVar :: Loc -> Maybe Var
getLocVar (Fresh v) = Just v
getLocVar (Fixed v) = Just v
getLocVar Top = Nothing
getLocVar l = error $"getLocVar: expected a single packed value location, got: "
                    ++show(doc l)
             


-- | We extend the environment when going under lexical binders, which
-- always have fixed abstract locations associated with them.
extendLocEnv :: [(Var,L1.Ty)] -> LocEnv -> SyM LocEnv
extendLocEnv []    e     = return e
extendLocEnv ((v,t):r) e =
    do t' <- tyWithFreshLocs t -- Temp, just to call argtyToLoc.
       extendLocEnv r (M.insert v (argtyToLoc (mangle v) t') e)


-- FIXME: Remove:
mapExprs :: (Env2 (Ty1 ()) -> Exp -> Exp) -> Prog -> Prog
mapExprs fn (Prog dd fundefs mainExp) =
    Prog dd
         (fmap (\ (FunDef nm arrTy@(ArrowTy inT _ _) arg bod) ->
                 let env = Env2 (M.singleton arg (fmap (\_->()) inT))
                                funEnv
                 in FunDef nm arrTy arg (fn env bod))
            fundefs)
         -- The function is implicitly assumed not to change the type!
         -- TODO: perhaps should re-infer the type here?
         (fmap (\(e,t) -> (fn (Env2 M.empty funEnv) e, t) ) mainExp)
  where
    -- FIXME: use progToEnv
    funEnv = M.fromList [ (n,(fmap (\_->()) a, fmap (\_->()) b))
                        | FunDef n (ArrowTy a _ b) _ _ <- M.elems fundefs ]

-- | Map exprs with an initial type environment:
mapMExprs :: Monad m => (Env2 (Ty1 ()) -> Exp -> m Exp) -> Prog -> m Prog
mapMExprs fn (Prog dd fundefs mainExp) =
    Prog dd <$>
         (mapM (\ (FunDef nm arrTy@(ArrowTy inT _ _) arg bod) ->
                 let env = Env2 (M.singleton arg (fmap (\_->()) inT))
                                funEnv
                 in FunDef nm arrTy arg <$> (fn env bod))
            fundefs)
         <*>
         (mapM (\ (e,t) ->
                 (,t) <$> fn (Env2 M.empty funEnv) e) mainExp)
  where
    -- FIXME: use progToEnv
    funEnv = M.fromList [ (n,(fmap (\_->()) a, fmap (\_->()) b))
                        | FunDef n (ArrowTy a _ b) _ _ <- M.elems fundefs ]
             
    
--------------------------------------------------------------------------------

