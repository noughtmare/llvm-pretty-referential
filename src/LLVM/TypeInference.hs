{-# LANGUAGE ViewPatterns #-}
module LLVM.TypeInference where

import LLVM.Types
import Data.Word
import Data.List
-- import Debug.Trace

class HasType a where
  getType :: a -> Type

-- We assume the instruction is well-typed
instance HasType Instr where
  getType Ret {} = PrimType Void
  getType RetVoid = PrimType Void
  getType (Arith _ Value { valType = t } _) = t
  getType (Bit _ Value { valType = t } _) = t
  getType (Conv _ _ t) = t
  getType (Call _ (PtrTo (FunTy t _ _)) _ _) = t
  getType Call {} = error "Call type must be a pointer to a function"
  getType (Alloca t _ _) = PtrTo t
  getType (Load v@Value { valType = PtrTo t } _ _) = t
  getType Load {} = error "Load argument should be a pointer"
  getType Store {} = PrimType Void
  getType Fence {} = PrimType Void
  getType (CmpXchg _ _ Value { valType = PtrTo t } _ _ _ _ _) = t
  getType CmpXchg {} = error "CmpXchg argument should be pointer"
  getType (AtomicRW _ _ Value { valType = PtrTo t } _ _ _) = t
  getType AtomicRW {} = error "AtomicRW argument should be pointer"
  getType (ICmp _ Value { valType = Vector n _ } _) = Vector n (PrimType (Integer 1))
  getType ICmp {} = PrimType (Integer 1)
  getType (FCmp _ Value { valType = Vector n _ } _) = Vector n (PrimType (Integer 1))
  getType FCmp {} = PrimType (Integer 1)
  getType (Phi t _) = t
  -- getType (GEP _ Value { valType = t'@(PtrTo t), valValue = ValIdent (IdentValStmt i) } tvs)
  --   = trace (show (t', t, stmtInstr i))
  --   $ getElementType Nothing t (map (\Value { valType = t', valValue = v} -> (t', v)) tvs)
  getType (GEP _ Value { valType = t@PtrTo{} } tvs)
    = PtrTo $ getElementType Nothing t (map (\Value { valType = t', valValue = v} -> (t', v)) tvs)
  getType GEP {} = error "GEP argument should be pointer"
  getType (Select _ Value { valType = t } _) = t
  getType (ExtractValue Value { valType = t } ixs)
    = getElementType Nothing t (map (\i -> (PrimType (Integer 32), ValInteger (fromIntegral i))) ixs)
  getType (InsertValue Value { valType = t } _ _) = t
  getType (ExtractElt Value { valType = Vector _ t } _) = t
  getType ExtractElt {} = error "ExtractElt argument should be vector"
  getType (InsertElt Value { valType = t } _ _) = t
  getType (ShuffleVector Value {valType = Vector _ t} _ Value { valType = Vector n _ })
    = Vector n t
  getType ShuffleVector {} = error "ShuffleVector arguments should be vectors"
  getType Jump {} = PrimType Void
  getType Br {} = PrimType Void
  getType (Invoke (FunTy t _ _) _ _ _ _) = t
  getType Invoke {} = error "Invoke type must be FunTy"
  getType Comment {} = PrimType Void -- not even really an instruction...
  getType Unreachable = PrimType Void
  getType Unwind = PrimType Void
  getType (VaArg _ t) = t
  getType IndirectBr {} = PrimType Void
  getType Switch {} = PrimType Void
  getType (LandingPad t _ _ _) = t
  getType Resume {} = PrimType Void

instance HasType IdentValue where
  getType (IdentValStmt x) = getType x
  getType (IdentValArgument x) = getType x

instance HasType SymValue where
  getType (SymValAlias x) = PtrTo (getType x)
  getType (SymValGlobal x) = PtrTo (getType x)
  getType (SymValDeclare x) = PtrTo (getType x)
  getType (SymValDefine x) = PtrTo (getType x)

instance HasType Stmt where
  getType = getType . stmtInstr

instance HasType Define where
  getType Define { defRetType=ret, defArgs=args } = makeFunTy ret (map argType args)

instance HasType Declare where
  getType Declare { decRetType=ret, decArgs=args } = makeFunTy ret args

instance HasType Global where
  getType Global { globalType = t } = t

instance HasType GlobalAlias where
  getType GlobalAlias { aliasType = t } = t

instance HasType Argument where
  getType Argument { argType = t } = t

instance HasType BasicBlock where
  getType BasicBlock { } = PrimType Label

instance HasType Value where
  getType v = valType v

getElementType :: Maybe Word64 -> Type -> [(Type, Value')] -> Type
getElementType v t ts@[(_,ValIdent (IdentValStmt i))] = {- trace ("getElementType valident: " ++ show (v, t, stmtInstr i)) $ -} getElementType' v t ts
getElementType v t ts = {- trace ("getElementType: " ++ show (v, t, ts)) $ -} getElementType' v t ts

getElementType' :: Maybe Word64 -> Type -> [(Type, Value')] -> Type
getElementType' v t [] = maybe t (`Vector` t) v
getElementType' v (Array _ t) ((x, _):xs)
  = getElementType' (case x of Vector n _ -> Just n; _ -> v) t xs
-- Maybe this misses global constants
getElementType' v (Struct _ ts _) ((_, ValInteger i):xs)
  = getElementType' v (genericIndex ts i) xs
getElementType' _ Struct {} _ = error "Struct index must be constant i32"
getElementType' v (PtrTo t) ((x, _):xs)
  = getElementType' (case x of Vector n _ -> Just n; _ -> v) t xs
getElementType' v (Vector _ t) ((x, _):xs)
  = getElementType' (case x of Vector n _ -> Just n; _ -> v) t xs
getElementType' v t ts = error $ "getElementType failed: " ++ show (v, t, ts)

makeFunTy :: Type -> [Type] -> Type
makeFunTy t@(PtrTo FunTy{}) _ = t
makeFunTy t@FunTy{} _ = t
makeFunTy ret args = FunTy ret args False
