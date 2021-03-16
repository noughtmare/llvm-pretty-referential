{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordPuns #-}
{-# LANGUAGE FlexibleInstances #-}
module LLVM.Conversion where

import           Data.Maybe                     ( fromMaybe )
import           LLVM.Types
import qualified Text.LLVM.AST                 as P

import           Data.Bifunctor                 ( Bifunctor(bimap, second) )
import           Data.Map                       ( Map )

type SymEnv = [(P.Symbol, SymValue)]
type IdentEnv = [(P.Ident, IdentValue)]
type LabEnv lab = [(lab, BasicBlock)]
type TypeEnv = [(P.Ident, Type)]

type Envs lab = (SymEnv, IdentEnv, LabEnv lab, TypeEnv)

resolveModule :: P.Module -> Module
resolveModule m = resolve (extractEnvs m) m

extractEnvs :: P.Module -> (SymEnv, IdentEnv, LabEnv P.BlockLabel, TypeEnv)
extractEnvs P.Module { P.modAliases, P.modGlobals, P.modDeclares, P.modDefines, P.modTypes }
    = res
  where
    symRes = concat
        [ [ (aliasName, SymValAlias (resolve res x))
          | x@P.GlobalAlias { P.aliasName } <- modAliases
          ]
        , [ (globalSym, SymValGlobal (resolve res x))
          | x@P.Global { P.globalSym } <- modGlobals
          ]
        , [ (decName, SymValDeclare (resolve typeRes x))
          | x@P.Declare { P.decName } <- modDeclares
          ]
        , [ (defName, SymValDefine (resolve res x))
          | x@P.Define { P.defName } <- modDefines
          ]
        ]
    identRes =
        [ ( typedValue
          , IdentValArgument
              (Argument (resolve res x) typedValue (resolve typeRes typedType))
          )
        | x@P.Define { P.defArgs }              <- modDefines
        , P.Typed { P.typedValue, P.typedType } <- defArgs
        ]
        ++ [ (ident, IdentValInstr (resolve res instr))
           | P.Define { P.defBody } <- modDefines
           , P.BasicBlock _ stmts   <- defBody
           , P.Result ident instr _ <- stmts
           ]
    labRes =
        [ (lab, resolve res x)
        | P.Define { P.defBody }                  <- modDefines
        , x@P.BasicBlock { P.bbLabel = Just lab } <- defBody
        ]
    typeRes =
        [ (typeName, resolve typeRes typeValue)
        | P.TypeDecl { P.typeName, P.typeValue } <- modTypes
        ]
    res = (symRes, identRes, labRes, typeRes)

class Resolve a where
  type Env a
  type Resolved a
  resolve :: Env a -> a -> Resolved a

instance Resolve P.Module where
    type Env P.Module = Envs P.BlockLabel
    type Resolved P.Module = Module
    resolve env@(_, _, _, typeEnv) = \case
        P.Module x1 x2 x3 x4 x5 x6 (map g -> x7) (map decls -> x8) (map defs -> x9) x10 (map ga -> x11)
            -> Module x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11
      where
        g     = resolve env
        decls = resolve typeEnv
        defs  = resolve env
        ga    = resolve env

instance Resolve P.GlobalAlias where
    type Env P.GlobalAlias = Envs P.BlockLabel
    type Resolved P.GlobalAlias = GlobalAlias
    resolve env@(_, _, _, typeEnv) = \case
        P.GlobalAlias x1 (t -> x2) (v -> x3) -> GlobalAlias x1 x2 x3
      where
        t = resolve typeEnv
        v = resolve env

instance Resolve P.Global where
    type Env P.Global = Envs P.BlockLabel
    type Resolved P.Global = Global
    resolve env@(_, _, _, typeEnv) = \case
        P.Global x1 x2 (t -> x3) (fmap v -> x4) x5 (gmda -> x6) ->
            Global x1 x2 x3 x4 x5 x6
      where
        t    = resolve typeEnv
        v    = resolve env
        gmda = resolve env

instance Eq lab => Resolve (Map P.KindMd (P.ValMd' lab)) where
    type Env (Map P.KindMd (P.ValMd' lab)) = Envs lab
    type Resolved (Map P.KindMd (P.ValMd' lab)) = Map P.KindMd ValMd
    resolve env = fmap (resolve env)

instance Resolve P.Declare where
    type Env P.Declare = TypeEnv
    type Resolved P.Declare = Declare
    resolve typeEnv = \case
        P.Declare (t -> x1) x2 (map t -> x3) x4 x5 x6 ->
            Declare x1 x2 x3 x4 x5 x6
        where t = resolve typeEnv

instance Resolve P.Define where
    type Env P.Define = Envs P.BlockLabel
    type Resolved P.Define = Define
    resolve env@(_, _, _, typeEnv) = \case
        P.Define x1 (t -> x2) x3 (map ti -> x4) x5 x6 x7 x8 (map (resolve env) -> x9) (fmda -> x10) x11
            -> Define x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11
      where
        t    = resolve typeEnv
        fmda = fmap (resolve env)
        ti (P.Typed typ x) = Typed (t typ) x

instance (Eq ident, Show ident) => Resolve (P.Type' ident) where
    type Env (P.Type' ident) = [(ident, Type)]
    type Resolved (P.Type' ident) = Type
    resolve env = \case
        P.PrimType x                       -> PrimType x
        P.Alias    (ident -> x)            -> Alias x
        P.Array x1 (t -> x2)               -> Array x1 x2
        P.FunTy (t -> x1) (map t -> x2) x3 -> FunTy x1 x2 x3
        P.PtrTo        (t -> x    )        -> PtrTo x
        P.Struct       (map t -> x)        -> Struct x
        P.PackedStruct (map t -> x)        -> PackedStruct x
        P.Vector x1 (t -> x2)              -> Vector x1 x2
        P.Opaque                           -> Opaque
      where
        t = resolve env
        ident i = fromMaybe (error "Cannot resolve ident") (lookup i env)

instance Resolve a => Resolve (P.Typed a) where
    type Env (P.Typed a) = (TypeEnv, Env a)
    type Resolved (P.Typed a) = Typed (Resolved a)
    resolve (typeEnv, aEnv) (P.Typed t x) =
        Typed (resolve typeEnv t) (resolve aEnv x)

instance Eq lab => Resolve (P.Value' lab) where
    type Env (P.Value' lab) = Envs lab
    type Resolved (P.Value' lab) = Value
    resolve env@(symEnv, identEnv, labEnv, typeEnv) = \case
        P.ValInteger x                      -> ValInteger x
        P.ValBool    x                      -> ValBool x
        P.ValFloat   x                      -> ValFloat x
        P.ValDouble  x                      -> ValDouble x
        P.ValFP80    x                      -> ValFP80 x
        P.ValIdent   (ident -> x)           -> ValIdent x
        P.ValSymbol  (sym -> x  )           -> ValSymbol x
        P.ValNull                           -> ValNull
        P.ValArray  (t -> x1) (map v -> x2) -> ValArray x1 x2
        P.ValVector (t -> x1) (map v -> x2) -> ValVector x1 x2
        P.ValStruct       (map tv -> x)     -> ValStruct x
        P.ValPackedStruct (map tv -> x)     -> ValPackedStruct x
        P.ValString       x                 -> ValString x
        P.ValConstExpr    (ce -> x)         -> ValConstExpr x
        P.ValUndef                          -> ValUndef
        P.ValLabel (lab -> x)               -> ValLabel x
        P.ValZeroInit                       -> ValZeroInit
        P.ValAsm x1 x2 x3 x4                -> ValAsm x1 x2 x3 x4
        P.ValMd (md -> x)                   -> ValMd x
      where
        t  = resolve typeEnv
        tv = resolve (typeEnv, env)
        v  = resolve env
        ce = resolve env
        md = resolve env
        sym s = fromMaybe (error "Cannot resolve symbol") (lookup s symEnv)
        lab l = fromMaybe (error "Cannot resolve label") (lookup l labEnv)
        ident i = fromMaybe (error "Cannot resolve ident") (lookup i identEnv)

instance Eq lab => Resolve (P.ConstExpr' lab) where
    type Env (P.ConstExpr' lab) = Envs lab
    type Resolved (P.ConstExpr' lab) = ConstExpr
    resolve env@(symEnv, _, labEnv, typeEnv) = \case
        P.ConstGEP x1 x2 (fmap t -> x3) (map tv -> x4) -> ConstGEP x1 x2 x3 x4
        P.ConstConv   x1         (tv -> x2) (t -> x3 ) -> ConstConv x1 x2 x3
        P.ConstSelect (tv -> x1) (tv -> x2) (tv -> x3) -> ConstSelect x1 x2 x3
        P.ConstBlockAddr (sym -> x1) (lab -> x2)       -> ConstBlockAddr x1 x2
        P.ConstFCmp  x1 (tv -> x2) (tv -> x3)          -> ConstFCmp x1 x2 x3
        P.ConstICmp  x1 (tv -> x2) (tv -> x3)          -> ConstICmp x1 x2 x3
        P.ConstArith x1 (tv -> x2) (v -> x3 )          -> ConstArith x1 x2 x3
        P.ConstBit   x1 (tv -> x2) (v -> x3 )          -> ConstBit x1 x2 x3
      where
        t  = resolve typeEnv
        tv = resolve (typeEnv, env)
        v  = resolve env
        sym s = fromMaybe (error "Cannot resolve symbol") (lookup s symEnv)
        lab l = fromMaybe (error "Cannot resolve label") (lookup l labEnv)



instance Eq lab => Resolve (P.BasicBlock' lab) where
    type Env (P.BasicBlock' lab) = Envs lab
    type Resolved (P.BasicBlock' lab) = BasicBlock
    resolve env (P.BasicBlock _ stmts) = BasicBlock (map (resolve env) stmts)

instance Eq lab => Resolve (P.Stmt' lab) where
    type Env (P.Stmt' lab) = Envs lab
    type Resolved (P.Stmt' lab) = Stmt
    resolve env = \case
        P.Result _ instr x ->
            Result (resolve env instr) (fmap (fmap (resolve env)) x)
        P.Effect instr x ->
            Effect (resolve env instr) (fmap (fmap (resolve env)) x)

instance Eq lab => Resolve (P.Instr' lab) where
    type Env (P.Instr' lab) = Envs lab
    type Resolved (P.Instr' lab) = Instr
    resolve env@(_, _, labEnv, typeEnv) = \case
        P.Ret (tv -> x)                        -> Ret x
        P.RetVoid                              -> RetVoid
        P.Arith x1 (tv -> x2) (v -> x3)        -> Arith x1 x2 x3
        P.Bit   x1 (tv -> x2) (v -> x3)        -> Bit x1 x2 x3
        P.Conv  x1 (tv -> x2) (t -> x3)        -> Conv x1 x2 x3
        P.Call x1 (t -> x2) (v -> x3) (map tv -> x4) -> Call x1 x2 x3 x4
        P.Alloca (t -> x1 ) (fmap tv -> x2) x3 -> Alloca x1 x2 x3
        P.Load   (tv -> x1) x2              x3 -> Load x1 x2 x3
        P.Store (tv -> x1) (tv -> x2) x3 x4    -> Store x1 x2 x3 x4
        P.Fence x1 x2                          -> Fence x1 x2
        P.CmpXchg x1 x2 (tv -> x3) (tv -> x4) (tv -> x5) x6 x7 x8 ->
            CmpXchg x1 x2 x3 x4 x5 x6 x7 x8
        P.AtomicRW x1 x2 (tv -> x3) (tv -> x4) x5 x6 ->
            AtomicRW x1 x2 x3 x4 x5 x6
        P.ICmp x1 (tv -> x2) (v -> x3)                -> ICmp x1 x2 x3
        P.FCmp x1 (tv -> x2) (v -> x3)                -> FCmp x1 x2 x3
        P.Phi (t -> x1) (map (bimap v lab) -> x2)     -> Phi x1 x2
        P.GEP    x1         (tv -> x2) (map tv -> x3) -> GEP x1 x2 x3
        P.Select (tv -> x1) (tv -> x2) (v -> x3     ) -> Select x1 x2 x3
        P.ExtractValue (tv -> x1) x2                  -> ExtractValue x1 x2
        P.InsertValue (tv -> x1) (tv -> x2) x3        -> InsertValue x1 x2 x3
        P.ExtractElt (tv -> x1) (v -> x2)             -> ExtractElt x1 x2
        P.InsertElt (tv -> x1) (tv -> x2) (v -> x3)   -> InsertElt x1 x2 x3
        P.ShuffleVector (tv -> x1) (v -> x2) (tv -> x3) ->
            ShuffleVector x1 x2 x3
        P.Jump (lab -> x)                       -> Jump x
        P.Br (tv -> x1) (lab -> x2) (lab -> x3) -> Br x1 x2 x3
        P.Invoke (t -> x1) (v -> x2) (map tv -> x3) (lab -> x4) (lab -> x5) ->
            Invoke x1 x2 x3 x4 x5
        P.Comment x                             -> Comment x
        P.Unreachable                           -> Unreachable
        P.Unwind                                -> Unwind
        P.VaArg      (tv -> x1) (t -> x2      ) -> VaArg x1 x2
        P.IndirectBr (tv -> x1) (map lab -> x2) -> IndirectBr x1 x2
        P.Switch (tv -> x1) (lab -> x2) (map (second lab) -> x3) ->
            Switch x1 x2 x3
        P.LandingPad (t -> x1) (fmap tv -> x2) x3 (map cl -> x4) ->
            LandingPad x1 x2 x3 x4
        P.Resume (tv -> x) -> Resume x
      where
        t  = resolve typeEnv
        tv = resolve (typeEnv, env)
        v  = resolve env
        cl = resolve env
        lab l = fromMaybe (error "Cannot resolve label") (lookup l labEnv)


instance (Eq lab) => Resolve (P.ValMd' lab) where
    type Env (P.ValMd' lab) = Envs lab
    type Resolved (P.ValMd' lab) = ValMd
    resolve env@(_, _, _, typeEnv) = \case
        P.ValMdString    x                    -> ValMdString x
        P.ValMdValue     (tv -> x)            -> ValMdValue x
        P.ValMdRef       x                    -> ValMdRef x
        P.ValMdNode      (map (fmap md) -> x) -> ValMdNode x
        P.ValMdLoc       (dl -> x           ) -> ValMdLoc x
        P.ValMdDebugInfo (di -> x           ) -> ValMdDebugInfo x
      where
        tv = resolve (typeEnv, env)
        dl = resolve env
        di = resolve env
        md = resolve env

instance (Eq lab) => Resolve (P.DebugLoc' lab) where
    type Env (P.DebugLoc' lab) = Envs lab
    type Resolved (P.DebugLoc' lab) = DebugLoc
    resolve env = \case
        P.DebugLoc x1 x2 (md -> x3) (fmap md -> x4) x5 ->
            DebugLoc x1 x2 x3 x4 x5
        where md = resolve env


instance (Eq lab) => Resolve (P.DebugInfo' lab) where
    type Env (P.DebugInfo' lab) = Envs lab
    type Resolved (P.DebugInfo' lab) = DebugInfo
    resolve env = \case
        P.DebugInfoBasicType     (resolve () -> x ) -> DebugInfoBasicType x
        P.DebugInfoCompileUnit   (resolve env -> x) -> DebugInfoCompileUnit x
        P.DebugInfoCompositeType (resolve env -> x) -> DebugInfoCompositeType x
        P.DebugInfoDerivedType   (resolve env -> x) -> DebugInfoDerivedType x
        P.DebugInfoEnumerator x1 x2                 -> DebugInfoEnumerator x1 x2
        P.DebugInfoExpression (resolve () -> x)     -> DebugInfoExpression x
        P.DebugInfoFile       (resolve () -> x)     -> DebugInfoFile x
        P.DebugInfoGlobalVariable (resolve env -> x) ->
            DebugInfoGlobalVariable x
        P.DebugInfoGlobalVariableExpression (resolve env -> x) ->
            DebugInfoGlobalVariableExpression x
        P.DebugInfoLexicalBlock (resolve env -> x) -> DebugInfoLexicalBlock x
        P.DebugInfoLexicalBlockFile (resolve env -> x) ->
            DebugInfoLexicalBlockFile x
        P.DebugInfoLocalVariable (resolve env -> x) -> DebugInfoLocalVariable x
        P.DebugInfoSubprogram    (resolve env -> x) -> DebugInfoSubprogram x
        P.DebugInfoSubrange      (resolve () -> x ) -> DebugInfoSubrange x
        P.DebugInfoSubroutineType (resolve env -> x) ->
            DebugInfoSubroutineType x
        P.DebugInfoNameSpace (resolve env -> x) -> DebugInfoNameSpace x
        P.DebugInfoTemplateTypeParameter (resolve env -> x) ->
            DebugInfoTemplateTypeParameter x
        P.DebugInfoTemplateValueParameter (resolve env -> x) ->
            DebugInfoTemplateValueParameter x
        P.DebugInfoImportedEntity (resolve env -> x) ->
            DebugInfoImportedEntity x
        P.DebugInfoLabel (resolve env -> x) -> DebugInfoLabel x

instance Resolve P.DIBasicType where
    type Env P.DIBasicType = ()
    type Resolved P.DIBasicType = DIBasicType
    resolve () = \case
        P.DIBasicType x1 x2 x3 x4 x5 x6 -> DIBasicType x1 x2 x3 x4 x5 x6

instance Eq lab => Resolve (P.DICompileUnit' lab) where
    type Env (P.DICompileUnit' lab) = Envs lab
    type Resolved (P.DICompileUnit' lab) = DICompileUnit
    resolve env = \case
        P.DICompileUnit x1 (fmap md -> x2) x3 x4 x5 x6 x7 x8 (fmap md -> x9) (fmap md -> x10) (fmap md -> x11) (fmap md -> x12) (fmap md -> x13) (fmap md -> x14) x15 x16
            -> DICompileUnit x1
                             x2
                             x3
                             x4
                             x5
                             x6
                             x7
                             x8
                             x9
                             x10
                             x11
                             x12
                             x13
                             x14
                             x15
                             x16
        where md = resolve env

instance Eq lab => Resolve (P.DICompositeType' lab) where
    type Env (P.DICompositeType' lab) = Envs lab
    type Resolved (P.DICompositeType' lab) = DICompositeType
    resolve env = \case
        P.DICompositeType x1 x2 (fmap md -> x3) x4 (fmap md -> x5) (fmap md -> x6) x7 x8 x9 x10 (fmap md -> x11) x12 (fmap md -> x13) (fmap md -> x14) x15 (fmap md -> x16)
            -> DICompositeType x1
                               x2
                               x3
                               x4
                               x5
                               x6
                               x7
                               x8
                               x9
                               x10
                               x11
                               x12
                               x13
                               x14
                               x15
                               x16
        where md = resolve env

instance Eq lab => Resolve (P.DIDerivedType' lab) where
    type Env (P.DIDerivedType' lab) = Envs lab
    type Resolved (P.DIDerivedType' lab) = DIDerivedType
    resolve env = \case
        P.DIDerivedType x1 x2 (fmap md -> x3) x4 (fmap md -> x5) (fmap md -> x6) x7 x8 x9 x10 (fmap md -> x11)
            -> DIDerivedType x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11
        where md = resolve env

instance Resolve P.DIExpression where
    type Env P.DIExpression = ()
    type Resolved P.DIExpression = DIExpression
    resolve () (P.DIExpression x) = DIExpression x

instance Resolve P.DIFile where
    type Env P.DIFile = ()
    type Resolved P.DIFile = DIFile
    resolve () (P.DIFile x1 x2) = DIFile x1 x2

instance Eq lab => Resolve (P.DIGlobalVariable' lab) where
    type Env (P.DIGlobalVariable' lab) = Envs lab
    type Resolved (P.DIGlobalVariable' lab) = DIGlobalVariable
    resolve env = \case
        P.DIGlobalVariable (fmap md -> x1) x2 x3 (fmap md -> x4) x5 (fmap md -> x6) x7 x8 (fmap md -> x9) (fmap md -> x10) x11
            -> DIGlobalVariable x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11
        where md = resolve env

instance Eq lab => Resolve (P.DIGlobalVariableExpression' lab) where
    type Env (P.DIGlobalVariableExpression' lab) = Envs lab
    type Resolved (P.DIGlobalVariableExpression' lab) = DIGlobalVariableExpression
    resolve env = \case
        P.DIGlobalVariableExpression (fmap md -> x1) (fmap md -> x2) ->
            DIGlobalVariableExpression x1 x2
        where md = resolve env

instance Eq lab => Resolve (P.DILexicalBlock' lab) where
    type Env (P.DILexicalBlock' lab) = Envs lab
    type Resolved (P.DILexicalBlock' lab) = DILexicalBlock
    resolve env = \case
        P.DILexicalBlock (fmap md -> x1) (fmap md -> x2) x3 x4 ->
            DILexicalBlock x1 x2 x3 x4
        where md = resolve env

instance Eq lab => Resolve (P.DILocalVariable' lab) where
    type Env (P.DILocalVariable' lab) = Envs lab
    type Resolved (P.DILocalVariable' lab) = DILocalVariable
    resolve env = \case
        P.DILocalVariable (fmap md -> x1) x2 (fmap md -> x3) x4 (fmap md -> x5) x6 x7
            -> DILocalVariable x1 x2 x3 x4 x5 x6 x7
        where md = resolve env

instance Eq lab => Resolve (P.DILexicalBlockFile' lab) where
    type Env (P.DILexicalBlockFile' lab) = Envs lab
    type Resolved (P.DILexicalBlockFile' lab) = DILexicalBlockFile
    resolve env = \case
        P.DILexicalBlockFile (md -> x1) (fmap md -> x2) x3 ->
            DILexicalBlockFile x1 x2 x3
        where md = resolve env

instance Eq lab => Resolve (P.DISubprogram' lab) where
    type Env (P.DISubprogram' lab) = Envs lab
    type Resolved (P.DISubprogram' lab) = DISubprogram
    resolve env = \case
        P.DISubprogram (fmap md -> x1) x2 x3 (fmap md -> x4) x5 (fmap md -> x6) x7 x8 x9 (fmap md -> x10) x11 x12 x13 x14 x15 (fmap md -> x16) (fmap md -> x17) (fmap md -> x18) (fmap md -> x19) (fmap md -> x20)
            -> DISubprogram x1
                            x2
                            x3
                            x4
                            x5
                            x6
                            x7
                            x8
                            x9
                            x10
                            x11
                            x12
                            x13
                            x14
                            x15
                            x16
                            x17
                            x18
                            x19
                            x20
        where md = resolve env

instance Resolve P.DISubrange where
    type Env P.DISubrange = ()
    type Resolved P.DISubrange = DISubrange
    resolve () (P.DISubrange x1 x2) = DISubrange x1 x2

instance Eq lab => Resolve (P.DISubroutineType' lab) where
    type Env (P.DISubroutineType' lab) = Envs lab
    type Resolved (P.DISubroutineType' lab) = DISubroutineType
    resolve env = \case
        P.DISubroutineType x1 (fmap md -> x2) -> DISubroutineType x1 x2
        where md = resolve env

instance Eq lab => Resolve (P.DINameSpace' lab) where
    type Env (P.DINameSpace' lab) = Envs lab
    type Resolved (P.DINameSpace' lab) = DINameSpace
    resolve env = \case
        P.DINameSpace x1 (md -> x2) (md -> x3) x4 -> DINameSpace x1 x2 x3 x4
        where md = resolve env

instance Eq lab => Resolve (P.DITemplateTypeParameter' lab) where
    type Env (P.DITemplateTypeParameter' lab) = Envs lab
    type Resolved (P.DITemplateTypeParameter' lab) = DITemplateTypeParameter
    resolve env = \case
        P.DITemplateTypeParameter x1 (fmap md -> x2) ->
            DITemplateTypeParameter x1 x2
        where md = resolve env

instance Eq lab => Resolve (P.DITemplateValueParameter' lab) where
    type Env (P.DITemplateValueParameter' lab) = Envs lab
    type Resolved (P.DITemplateValueParameter' lab) = DITemplateValueParameter
    resolve env = \case
        P.DITemplateValueParameter x1 x2 (fmap md -> x3) (md -> x4) ->
            DITemplateValueParameter x1 x2 x3 x4
        where md = resolve env

instance Eq lab => Resolve (P.DIImportedEntity' lab) where
    type Env (P.DIImportedEntity' lab) = Envs lab
    type Resolved (P.DIImportedEntity' lab) = DIImportedEntity
    resolve env = \case
        P.DIImportedEntity x1 (fmap md -> x2) (fmap md -> x3) (fmap md -> x4) x5 x6
            -> DIImportedEntity x1 x2 x3 x4 x5 x6
        where md = resolve env

instance Eq lab => Resolve (P.DILabel' lab) where
    type Env (P.DILabel' lab) = Envs lab
    type Resolved (P.DILabel' lab) = DILabel
    resolve env = \case
        P.DILabel (fmap md -> x1) x2 (fmap md -> x3) x4 -> DILabel x1 x2 x3 x4
        where md = resolve env

instance Eq lab => Resolve (P.Clause' lab) where
    type Env (P.Clause' lab) = Envs lab
    type Resolved (P.Clause' lab) = Clause
    resolve env@(_, _, _, typeEnv) = \case
        P.Catch  (tv -> x) -> Catch x
        P.Filter (tv -> x) -> Filter x
        where tv = resolve (typeEnv, env)
