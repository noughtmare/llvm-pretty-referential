{-# LANGUAGE ViewPatterns #-}
module LLVM.TypeInference where

import LLVM.Types
import Data.Word
import Data.List

class HasType a where
  getType :: a -> Type

-- We assume the instruction is well-typed
instance HasType Instr where
  getType Ret {} = PrimType Void
  getType RetVoid = PrimType Void
  getType (Arith _ Value { valType = t } _) = t
  getType (Bit _ Value { valType = t } _) = t
  getType (Conv _ _ t) = t
  getType (Call _ (FunTy t _ _) _ _) = t
  getType (Call _ _ _ _) = error "Call type must be FunTy"
  getType (Alloca t _ _) = PtrTo t
  getType (Load Value { valType = t } _ _) = t
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
  getType (GEP _ Value { valType = PtrTo t } tvs)
    = getElementType Nothing t (map (\Value { valType = t', valValue = v} -> (t', v)) tvs)
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
  getType (Invoke _ _ _ _ _) = error "Invoke type must be FunTy"
  getType Comment {} = PrimType Void -- not even really an instruction...
  getType Unreachable = PrimType Void
  getType Unwind = PrimType Void
  getType (VaArg _ t) = t
  getType IndirectBr {} = PrimType Void
  getType Switch {} = PrimType Void
  getType (LandingPad t _ _ _) = t
  getType Resume {} = PrimType Void

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

getElementType :: Maybe Word64 -> Type -> [(Type, Value')] -> Type
getElementType v t [] = maybe t (\n -> Vector n t) v
getElementType v (Array _ t) ((x, _):xs)
  = getElementType (case x of Vector n _ -> Just n; _ -> v) t xs
-- Maybe this misses global constants
getElementType v (Struct _ ts _) ((_, ValInteger i):xs)
  = getElementType v (genericIndex ts i) xs
getElementType _ Struct {} _ = error "Struct index must be constant i32"
getElementType _ _ _ = error "getElementType failed"

makeFunTy :: Type -> [Type] -> Type
makeFunTy t@FunTy {} _ = t
makeFunTy ret args = FunTy ret args False
