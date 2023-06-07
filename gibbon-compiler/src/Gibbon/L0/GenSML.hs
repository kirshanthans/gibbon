module Gibbon.L0.GenSML where

import Gibbon.L0.Syntax
import Gibbon.Common

import Text.PrettyPrint hiding ((<>))

-- not quite sure what to do for these
ppExt :: E0Ext Ty0 Ty0 -> Doc
ppExt ex = case ex of
  LambdaE x0 pe -> 
    parens $ hsep
      [ hsep $ ppVar . fst <$> x0
      , text "=>"
      , ppPreExp pe
      ]
  PolyAppE pe pe' -> 
    hsep $ parens . ppPreExp <$> [pe, pe']
  FunRefE ty0s var -> _
  BenchE var ty0s pes b -> _
  ParE0 pes -> _
  PrintPacked ty0 pe -> _
  CopyPacked ty0 pe -> _
  TravPacked ty0 pe -> _
  L loc pe -> _
  LinearExt le -> _

ppPreExp :: PreExp E0Ext Ty0 Ty0 -> Doc
ppPreExp pe = case pe of
  VarE (Var s) -> text $ show s
  LitE n -> text $ show n
  CharE c -> char c
  FloatE x -> text $ show x
  LitSymE (Var s) -> quotes $ text $ show s
  AppE var _ pes -> ppApp (ppVar var) pes
  PrimAppE pr pes -> ppPrim pr pes
  LetE x0 pe' -> _
  IfE pe' pe2 pe3 -> _
  MkProdE pes -> _
  ProjE n pe' -> _
  CaseE pe' x0 -> _
  DataConE ty0 s pes -> _
  TimeIt pe' ty0 b -> _
  WithArenaE var pe' -> _
  SpawnE var ty0s pes -> _
  SyncE -> _
  MapE x0 pe' -> _
  FoldE x0 x1 pe' -> _
  Ext ee -> _

ppApp :: Doc -> [PreExp E0Ext Ty0 Ty0] -> Doc
ppApp var pes = hsep $ var : (ppPreExp <$> pes)

ppPrim :: Prim Ty0 -> [PreExp E0Ext Ty0 Ty0] -> Doc
ppPrim pr pes = case pr of
  AddP -> binary "+" pes
  SubP -> binary "-" pes
  MulP -> binary "*" pes
  DivP -> binary "div" pes
  ModP -> binary "mod" pes
  ExpP -> binary "**" pes
  RandP -> ppApp (text "Random.randInt") pes
  EqIntP -> binary "=" pes
  LtP -> binary "<" pes
  GtP -> binary ">" pes
  LtEqP -> binary "<=" pes
  GtEqP -> binary ">=" pes
  FAddP -> binary "+" pes
  FSubP -> binary "-" pes
  FMulP -> binary "*" pes
  FDivP -> binary "/" pes
  FExpP -> 
    let
      (l, r) = extractBinary "pow" pes
    in
    parens $ hsep
      [ "Math.pow"
      , parens $ hcat [l, comma, r]
      ]
  FRandP -> ppApp (text "Random.randFloat") pes
  EqFloatP -> binary "=" pes
  EqCharP -> binary "=" pes
  FLtP -> binary "<" pes
  FGtP -> binary ">" pes
  FLtEqP -> binary "<=" pes
  FGtEqP -> binary ">=" pes
  FSqrtP -> ppApp (text "Math.sqrt") pes
  IntToFloatP -> ppApp (text "Real.fromInt") pes
  FloatToIntP -> ppApp (text "Int.fromReal") pes
  FTanP -> ppApp (text "Math.tan") pes
  EqSymP -> binary "=" pes
  EqBenchProgP _ -> error "GenSML: EqBenchProgP"
  OrP -> binary "orelse" pes
  AndP -> binary "andalso" pes
  MkTrue -> text "true"
  MkFalse -> text "false"
  ErrorP s _ -> hsep [text "raise", quotes $ text s]
  SizeParam -> error "SizeParam"
  IsBig -> error "IsBig"
  GetNumProcessors -> error "GetNumProcessors"
  PrintInt -> ppApp (text "print") pes <> semi
  PrintChar -> ppApp (text "print") pes <> semi
  PrintFloat -> ppApp (text "print") pes <> semi
  PrintBool -> 
    ppApp (text "(fn true => \"True\" | false => \"False\")") pes <> semi
  PrintSym -> ppApp (text "print") pes <> semi
  ReadInt -> error "ReadInt"  -- Have every program read from stdin?
  DictInsertP _ -> empty  -- Simulate all this?
  DictLookupP _ -> empty
  DictEmptyP _ -> empty
  DictHasKeyP _ -> empty
  SymSetEmpty -> empty
  SymSetInsert -> empty
  SymSetContains -> empty
  SymHashEmpty -> empty
  SymHashInsert -> empty
  SymHashLookup -> empty
  SymHashContains -> empty
  IntHashEmpty -> empty
  IntHashInsert -> empty
  IntHashLookup -> empty
  PDictAllocP ty0 ty0' -> _
  PDictInsertP ty0 ty0' -> _
  PDictLookupP ty0 ty0' -> _
  PDictHasKeyP ty0 ty0' -> _
  PDictForkP ty0 ty0' -> _
  PDictJoinP ty0 ty0' -> _
  LLAllocP ty0 -> _
  LLIsEmptyP ty0 -> _
  LLConsP ty0 -> _
  LLHeadP ty0 -> _
  LLTailP ty0 -> _
  LLFreeP ty0 -> _
  LLFree2P ty0 -> _
  LLCopyP ty0 -> _
  VAllocP ty0 -> _
  VFreeP ty0 -> _
  VFree2P ty0 -> _
  VLengthP ty0 -> _
  VNthP ty0 -> _
  VSliceP ty0 -> _
  InplaceVUpdateP ty0 -> _
  VConcatP ty0 -> _
  VSortP ty0 -> _
  InplaceVSortP ty0 -> _
  VMergeP ty0 -> _
  Write3dPpmFile s -> _
  ReadPackedFile m_s s m_var ty0 -> _
  WritePackedFile s ty0 -> _
  ReadArrayFile ma ty0 -> _
  RequestEndOf -> _
  RequestSizeOf -> _
  Gensym -> _

ppVar :: Var -> Doc
ppVar (Var s) = text $ show s

-- interleave :: Doc -> [Doc] -> Doc
-- interleave sepr lst = case lst of
--   [] -> empty
--   d : ds -> d <+> foldr (\x -> (sepr <+> x <>)) empty ds

    -- es@[_, _] -> parens $ interleave (text "+") $ ppPreExp <$> es
    -- es -> error $ mconcat 
    --   [ "L0 error: AddP is provided "
    --   , show $ length es
    --   , " arguments"
    --   ]

binary :: String -> [PreExp E0Ext Ty0 Ty0] -> Doc
binary opSym pes = 
  parens $ hsep [l, text opSym, r]
  where
    (l, r) = extractBinary opSym pes

extractBinary :: String -> [PreExp E0Ext Ty0 Ty0] -> (Doc, Doc)
extractBinary opSym pes = case ppPreExp <$> pes of
  [l, r] -> (l, r)
  es -> error $ mconcat
    [ "L0 error: (", opSym, ") is provided "
    , show $ length es, " arguments"
    ]

extractUnary :: String -> [PreExp E0Ext Ty0 Ty0] -> Doc
extractUnary opSym pes = case ppPreExp <$> pes of
  [x] -> x
  es -> error $ mconcat
    [ "L0 error: (", opSym, ") is provided "
    , show $ length es, " arguments"
    ]
