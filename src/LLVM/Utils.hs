-- {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module LLVM.Utils where

import LLVM.TypeInference
import LLVM.Types
import Data.List (find, elemIndex)
import Text.Regex.TDFA ((=~))

import Debug.Trace

class IsValue a where
  toValue :: a -> Value

instance IsValue Value where
  toValue = id

-- instance IsValue IdentValue where
--   valueContent iv = ValIdent iv

instance IsValue IdentValue where
  toValue (IdentValStmt x) = toValue x
  toValue (IdentValArgument x) = toValue x

instance IsValue Stmt where
  toValue x = Value (getType x)
                    (uniqueId x)
                    (ValIdent (IdentValStmt x))
                    (case stmtType x of
                       Result y -> Just (prettyIdent y)
                       Effect -> Nothing)

instance IsValue Argument where
  toValue x = Value (getType x)
                    (uniqueId x)
                    (ValIdent (IdentValArgument x))
                    (Just (argName x))

instance IsValue SymValue where
  toValue (SymValAlias ga) = toValue ga
  toValue (SymValGlobal g) = toValue g
  toValue (SymValDeclare dec) = toValue dec
  toValue (SymValDefine def) = toValue def

prettyValName :: Value -> Maybe String
prettyValName v = case valValue v of
  ValSymbol _ -> ("@" <>) <$> valName v
  ValIdent _ -> ("%" <>) <$> valName v
  ValLabel _ -> ("%" <>) <$> valName v
  _ -> valName v

prettySymbol :: Symbol -> String
prettySymbol (Symbol x) = "@" <> x

prettyIdent :: Ident -> String
prettyIdent (Ident x) = "%" <> x

prettyLabel :: BasicBlock -> String
prettyLabel BasicBlock {bbName = x} = "%" <> x

instance IsValue GlobalAlias where
  toValue ga = Value (getType ga)
                     (uniqueId ga)
                     (ValSymbol (SymValAlias ga))
                     (Just (prettySymbol (aliasName ga)))

instance IsValue Global where
  toValue g = Value (getType g)
                    (uniqueId g)
                    (ValSymbol (SymValGlobal g))
                    (Just (prettySymbol (globalName g)))

instance IsValue Declare where
  toValue dec = Value (getType dec)
                      (uniqueId dec)
                      (ValSymbol (SymValDeclare dec))
                      (Just (prettySymbol (decName dec)))

instance IsValue Define where
  toValue def = Value (getType def)
                      (uniqueId def)
                      (ValSymbol (SymValDefine def))
                      (Just (prettySymbol (defName def)))

instance IsValue BasicBlock where
  toValue x = Value (getType x)
                    (uniqueId x)
                    (ValLabel x)
                    (Just (prettyLabel x))

-- | Find a function in the Module by its name.
findFunctionByName :: Module -> String -> Maybe Define
findFunctionByName m s = find isFunc $ modDefines m
  where
    isFunc f = defName f == Symbol s

-- | Find the function named 'main' in the 'Module', if any.
findMain :: Module -> Maybe Define
findMain m = findFunctionByName m "main"

class FromValue a where
  fromValue :: Value -> Maybe a

instance FromValue Argument where
  fromValue v = case valValue v of
    ValIdent (IdentValArgument a) -> Just a
    _                             -> Nothing
instance FromValue Stmt where
  fromValue v = case valValue v of
    ValIdent (IdentValStmt s) -> Just s
    _                         -> Nothing

instance FromValue Define where
  fromValue v = case valValue v of
    ValSymbol (SymValDefine def) -> Just def
    _                            -> Nothing

instance FromValue Declare where
  fromValue v = case valValue v of
    ValSymbol (SymValDeclare dec) -> Just dec
    _                             -> Nothing

instance FromValue Global where
  fromValue v = case valValue v of
    ValSymbol (SymValGlobal g) -> Just g
    _                          -> Nothing

instance FromValue GlobalAlias where
  fromValue v = case valValue v of
    ValSymbol (SymValAlias ga) -> Just ga
    _                          -> Nothing

instance FromValue Instr where
  fromValue v = stmtInstr <$> fromValue v

{-# INLINABLE firstNonPhiStmt #-}
-- | Get the first statement in a basic block that is not a Phi
-- node.  This is total because basic blocks cannot be empty and must
-- end in a terminator statement (Phi nodes are not terminators).
firstNonPhiStmt :: BasicBlock -> Stmt
firstNonPhiStmt bb = i
  where
    i : _ = dropWhile stmtIsPhi $ bbStmts bb

{-# INLINABLE stmtIsPhi #-}
-- | Predicate to test an instruction to see if it is a phi node
stmtIsPhi :: Stmt -> Bool
stmtIsPhi v = case stmtInstr v of
  Phi {} -> True
  _ -> False

{-# INLINABLE isFirstNonPhiStmt #-}
-- | Determine if @i@ is the first non-phi instruction in its block.
isFirstNonPhiStmt :: Stmt -> Bool
isFirstNonPhiStmt i = i == firstNonPhiStmt bb
  where
    bb = stmtBasicBlock i

{-# INLINABLE stmtIsTerminator #-}
-- | Determine if an instruction is a Terminator instruction (i.e.,
-- ends a BasicBlock)
stmtIsTerminator :: Stmt -> Bool
stmtIsTerminator i = case stmtInstr i of
  Ret {} -> True
  RetVoid {} -> True
  Jump {} -> True
  Br {} -> True
  Switch {} -> True
  IndirectBr {} -> True
  Resume {} -> True
  Unreachable {} -> True
  Invoke {} -> True
  _ -> False

{-# INLINABLE stripBitcasts #-}
-- | Strip all wrapper bitcasts from a Value
stripBitcasts :: Value -> Value
stripBitcasts v = case valValue v of
  ValIdent (IdentValStmt Stmt {stmtInstr = Conv BitCast cv _}) -> cv
  ValConstExpr (ConstConv BitCast cv _) -> cv
  _ -> v

structBaseName :: String -> String
structBaseName s = let pfx : _ = captures in pfx
 where
  pattern :: String
  pattern = "([[:alpha:]]+\\.[:<> [:alnum:]_]+)(\\.[[:digit:]]+)*"
  m :: (String, String, String, [String])
  m                   = s =~ pattern
  (_, _, _, captures) = m

-- | Find the zero-based index into the argument list of the 'Function'
-- containing this 'Argument'.
argumentIndex :: Argument -> Int
argumentIndex a = ix
  where
    f = argDefine a
    Just ix = elemIndex a (defArgs f)

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _ = Nothing

-- | Get the ret instruction for a Function
defExitStmt :: Define -> Maybe Stmt
defExitStmt f = safeHead $ filter isRetInst is
 where
  is = concatMap bbStmts (defBody f)
  isRetInst :: Stmt -> Bool
  isRetInst i = case stmtInstr i of
    Ret{}     -> True
    RetVoid{} -> True
    _         -> False

-- | Get all exit instructions for a Function (ret, unreachable, unwind)
defExitStmts :: Define -> [Stmt]
defExitStmts f = filter isRetInst is
 where
  is = concatMap bbStmts (defBody f)
  isRetInst i = case stmtInstr i of
    Ret{}         -> True
    RetVoid{}     -> True
    Unreachable{} -> True
    _             -> False

-- | Take a type and remove all of its pointer wrappers
stripPointerTypes :: Type -> Type
stripPointerTypes t =
  case t of
    PtrTo t' -> stripPointerTypes t'
    _ -> t

-- | Strip off the struct. prefix and any .NNN suffixes added by LLVM
-- to a struct type name.  If the type is not a struct type, return
-- Nothing.
structTypeToName :: Type -> Maybe String
structTypeToName (Struct (Right (Ident n)) _ _) =
  return $ structBaseName n
structTypeToName _ = fail "NotStructType"
