-- {-# LANGUAGE FlexibleInstances #-}
module LLVM.Utils where

import LLVM.TypeInference
import LLVM.Types

class IsValue a where
  toValue :: a -> Value

instance IsValue Value where
  toValue = id

-- instance IsValue IdentValue where
--   valueContent iv = ValIdent iv

instance IsValue Stmt where
  toValue x = Value 
    { valType = getType x
    , valUniqueId = stmtUniqueId x
    , valValue = ValIdent (IdentValStmt x)
    }

instance IsValue SymValue where
  toValue (SymValAlias ga) = toValue ga
  toValue (SymValGlobal g) = toValue g
  toValue (SymValDeclare dec) = toValue dec
  toValue (SymValDefine def) = toValue def

instance IsValue GlobalAlias where
  toValue ga = Value (getType ga) (aliasUniqueId ga) (ValSymbol (SymValAlias ga))

instance IsValue Global where
  toValue g = Value (getType g) (globalUniqueId g) (ValSymbol (SymValGlobal g))

instance IsValue Declare where
  toValue dec = Value (getType dec) (decUniqueId dec) (ValSymbol (SymValDeclare dec))

instance IsValue Define where
  toValue def = Value (getType def) (defUniqueId def) (ValSymbol (SymValDefine def))

instance IsValue Argument where
  toValue arg = Value (getType arg) (argUniqueId arg) (ValIdent (IdentValArgument arg))
