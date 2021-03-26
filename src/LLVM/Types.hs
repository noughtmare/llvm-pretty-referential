{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
{-# OPTIONS_GHC -O0 #-}
module LLVM.Types
  ( module Text.LLVM.AST
  , UniqueId
  -- , Typed (..)
  , SymValue (..)
  , IdentValue (..)
  , Value (..)
  , Value' (..)
  , Type (..)
  , BasicBlock (..)
  , StmtType (..)
  , Stmt (..)
  , Clause (..)
  , ConstExpr (..)
  , Instr (..)
  , TypeDecl (..)
  , Global (..)
  , Define (..)
  , Declare (..)
  , GlobalAlias (..)
  , Module (..)
  , Argument (..)
  , ValMd (..)
  , DebugLoc (..)
  , DebugInfo (..)
  , DIFile (..)
  , DILabel (..)
  , DIFlags
  , DwarfTag
  , DwarfLang
  , DwarfVirtuality
  , DwarfAttrEncoding
  , DISubrange (..)
  , DIBasicType (..)
  , DIExpression (..)
  , DINameSpace (..)
  , DISubprogram (..)
  , DICompileUnit (..)
  , DIDerivedType (..)
  , DILexicalBlock (..)
  , DIEmissionKind
  , DICompositeType (..)
  , DILocalVariable (..)
  , DIGlobalVariable (..)
  , DISubroutineType (..)
  , DIImportedEntity (..)
  , DILexicalBlockFile (..)
  , DIGlobalVariableExpression (..)
  , DITemplateTypeParameter (..)
  , DITemplateValueParameter (..)
  , stripBitcasts
  ) where

import           Text.LLVM.AST                  ( Align
                                                , ArithOp (..)
                                                , AtomicOrdering (..)
                                                , AtomicRWOp (..)
                                                , BitOp (..)
                                                , ConvOp (..)
                                                , DataLayout
                                                , LayoutSpec (..)
                                                , FCmpOp (..)
                                                , FP80Value (..)
                                                , FunAttr (..)
                                                , GC (..)
                                                , GlobalAttrs (..)
                                                , ICmpOp (..)
                                                , Ident (..)
                                                , InlineAsm
                                                , KindMd
                                                , Linkage (..)
                                                , NamedMd (..)
                                                , PrimType (..)
                                                , FloatType (..)
                                                , SelectionKind (..)
                                                , Symbol (..)
                                                , UnnamedMd (..)
                                                , BlockLabel (..)
                                                )

import           Data.Map                       ( Map )

import           Data.Data                      ( Data )
import           Data.Int                       ( Int32
                                                , Int64
                                                )
import           Data.Word                      ( Word16
                                                , Word32
                                                , Word64
                                                , Word8
                                                )
import           GHC.Generics                   ( Generic )
import           Data.Hashable
import           Control.DeepSeq

-- TODO this is probably no longer true
--
-- This isn't very honest, but Values are part of Modules and
-- are fully evaluated before the module is constructed.
instance NFData Instr where
  rnf _ = ()
instance NFData Value where
  rnf _ = ()
instance NFData BasicBlock where
  rnf _ = ()
instance NFData Define where
  rnf _ = ()
instance NFData Argument where
  rnf _ = ()
instance NFData Type where
  rnf _ = ()

-- data Typed a = Typed
--   { typedType  :: Type
--   , typedValue :: a
--   }
--   deriving (Data, Eq, Ord, Generic, Show)

data SymValue
  = SymValAlias GlobalAlias
  | SymValGlobal Global
  | SymValDeclare Declare
  | SymValDefine Define
  deriving (Data, Generic, Eq, Ord, Show)

data IdentValue
  = IdentValArgument Argument
  | IdentValStmt Stmt
  deriving (Data, Generic, Eq, Ord, Show)

data Value = Value
  { valType :: Type
  , valUniqueId :: UniqueId
  , valValue :: Value'
  }
  deriving (Data, Generic, Eq, Ord, Show)

instance Hashable Value where
  hashWithSalt s = hashWithSalt s . valUniqueId

-- Resolve labels, identifiers and instructions
data Value'
  = ValInteger Integer
  | ValBool Bool
  | ValFloat Float
  | ValDouble Double
  | ValFP80 FP80Value
  | ValIdent IdentValue
  | ValSymbol SymValue
  | ValNull
  | ValArray Type [Value]
  | ValVector Type [Value]
  | ValStruct [Value] Bool
  | ValString [Word8]
  | ValConstExpr ConstExpr
  | ValUndef
  | ValLabel BasicBlock
  | ValZeroInit
  | ValAsm Bool Bool String String
  | ValMd ValMd
  deriving (Eq, Ord, Data, Generic, Show)

-- Resolve type names
data Type
  = PrimType PrimType
  | Array Word64 Type
  | FunTy Type [Type] Bool
  | PtrTo Type
  | Struct (Either UniqueId Ident) [Type] Bool
  | Vector Word64 Type
  | Opaque (Either UniqueId Ident)
  deriving (Eq, Ord, Data, Generic, Show)

instance Hashable Ident where
  hashWithSalt s (Ident i) = hashWithSalt s i

instance Hashable FloatType where
  hashWithSalt s = hashWithSalt s . fromEnum

instance Hashable PrimType where
  hashWithSalt s Label = s `hashWithSalt` (1 :: Int) 
  hashWithSalt s Void = s `hashWithSalt` (2 :: Int)
  hashWithSalt s (Integer i) = s `hashWithSalt` i `hashWithSalt` (3 :: Int)
  hashWithSalt s (FloatType f) = s `hashWithSalt` f `hashWithSalt` (4 :: Int)
  hashWithSalt s X86mmx = s `hashWithSalt` (5 :: Int)
  hashWithSalt s Metadata = s `hashWithSalt` (6 :: Int)

instance Hashable Type where
  hashWithSalt s (PrimType p) = s `hashWithSalt` p `hashWithSalt` (1 :: Int)
  hashWithSalt s (Array i t) = s `hashWithSalt` i `hashWithSalt` t `hashWithSalt` (2 :: Int)
  hashWithSalt s (FunTy t ts b) = s `hashWithSalt` t `hashWithSalt` ts `hashWithSalt` b `hashWithSalt` (3 :: Int)
  hashWithSalt s (PtrTo t) = s `hashWithSalt` t `hashWithSalt` (4 :: Int)
  hashWithSalt s (Struct (Right n) _ _) = s `hashWithSalt` n `hashWithSalt` (5 :: Int)
  hashWithSalt s (Struct (Left i) _ p) = s `hashWithSalt` i `hashWithSalt` p `hashWithSalt` (5 :: Int)
  hashWithSalt s (Vector n t) = s `hashWithSalt` n `hashWithSalt` t `hashWithSalt` (6 :: Int)
  hashWithSalt s (Opaque n) = s `hashWithSalt` n `hashWithSalt` (7 :: Int)

data BasicBlock = BasicBlock
  { bbName :: String
  , bbLabel :: Maybe BlockLabel
  , bbStmts :: [Stmt]
  , bbUniqueId :: UniqueId
  }
  deriving (Eq, Ord, Data, Generic, Show)

instance Hashable BasicBlock where
  hashWithSalt s = hashWithSalt s . bbUniqueId

data StmtType = Result Ident | Effect deriving (Eq, Ord, Data, Generic, Show)

data Stmt = Stmt
  { stmtType       :: StmtType
  , stmtInstr      :: Instr
  , stmtMd         :: [(String, ValMd)]
  , stmtUniqueId   :: UniqueId
  , stmtBasicBlock :: BasicBlock
  }
  deriving (Eq, Ord, Data, Generic, Show)

data Clause
  = Catch Value
  | Filter Value
  deriving (Eq, Ord, Data, Generic, Show)

data ConstExpr
  = ConstGEP Bool (Maybe Word64) (Maybe Type) [Value]
  -- ^ Element type introduced in LLVM 3.7
  | ConstConv ConvOp Value Type
  | ConstSelect Value Value Value
  | ConstBlockAddr SymValue BasicBlock
  | ConstFCmp FCmpOp Value Value
  | ConstICmp ICmpOp Value Value
  | ConstArith ArithOp Value Value
  | ConstBit BitOp Value Value
  deriving (Data, Eq, Generic, Ord, Show)

data ValMd
  = ValMdString String
  | ValMdValue Value
  | ValMdRef Int
  | ValMdNode [Maybe ValMd]
  | ValMdLoc DebugLoc
  | ValMdDebugInfo DebugInfo
  deriving (Eq, Ord, Data, Generic, Show)

data DebugLoc = DebugLoc
  { dlLine     :: Word32
  , dlCol      :: Word32
  , dlScope    :: ValMd
  , dlIA       :: Maybe ValMd
  , dlImplicit :: Bool
  }
  deriving (Data, Eq, Generic, Ord, Show)

data DebugInfo
  = DebugInfoBasicType DIBasicType
  | DebugInfoCompileUnit DICompileUnit
  | DebugInfoCompositeType DICompositeType
  | DebugInfoDerivedType DIDerivedType
  | DebugInfoEnumerator String !Int64
  | DebugInfoExpression DIExpression
  | DebugInfoFile DIFile
  | DebugInfoGlobalVariable DIGlobalVariable
  | DebugInfoGlobalVariableExpression DIGlobalVariableExpression
  | DebugInfoLexicalBlock DILexicalBlock
  | DebugInfoLexicalBlockFile DILexicalBlockFile
  | DebugInfoLocalVariable DILocalVariable
  | DebugInfoSubprogram DISubprogram
  | DebugInfoSubrange DISubrange
  | DebugInfoSubroutineType DISubroutineType
  | DebugInfoNameSpace DINameSpace
  | DebugInfoTemplateTypeParameter DITemplateTypeParameter
  | DebugInfoTemplateValueParameter DITemplateValueParameter
  | DebugInfoImportedEntity DIImportedEntity
  | DebugInfoLabel DILabel
  deriving (Data, Eq, Generic, Ord, Show)

data DILabel = DILabel
  { dilScope :: Maybe ValMd
  , dilName  :: String
  , dilFile  :: Maybe ValMd
  , dilLine  :: Word32
  }
  deriving (Data, Eq, Generic, Ord, Show)

data DIImportedEntity = DIImportedEntity
  { diieTag    :: DwarfTag
  , diieScope  :: Maybe ValMd
  , diieEntity :: Maybe ValMd
  , diieFile   :: Maybe ValMd
  , diieLine   :: Word32
  , diieName   :: Maybe String
  }
  deriving (Data, Eq, Generic, Ord, Show)

data DITemplateTypeParameter = DITemplateTypeParameter
  { dittpName :: Maybe String
  , dittpType :: Maybe ValMd
  }
  deriving (Data, Eq, Generic, Ord, Show)

data DITemplateValueParameter = DITemplateValueParameter
  { ditvpTag   :: DwarfTag
  , ditvpName  :: Maybe String
  , ditvpType  :: Maybe ValMd
  , ditvpValue :: ValMd
  }
  deriving (Data, Eq, Generic, Ord, Show)

data DINameSpace = DINameSpace
  { dinsName  :: Maybe String
  , dinsScope :: ValMd
  , dinsFile  :: ValMd
  , dinsLine  :: Word32
  }
  deriving (Data, Eq, Generic, Ord, Show)

-- TODO: Turn these into sum types
-- See https://github.com/llvm-mirror/llvm/blob/release_38/include/llvm/Support/Dwarf.def
type DwarfAttrEncoding = Word16
type DwarfLang = Word16
type DwarfTag = Word16
type DwarfVirtuality = Word8
-- See https://github.com/llvm-mirror/llvm/blob/release_38/include/llvm/IR/DebugInfoMetadata.h#L175
type DIFlags = Word32
-- This seems to be defined internally as a small enum, and defined
-- differently across versions. Maybe turn this into a sum type once
-- it stabilizes.
type DIEmissionKind = Word8

data DIBasicType = DIBasicType
  { dibtTag      :: DwarfTag
  , dibtName     :: String
  , dibtSize     :: Word64
  , dibtAlign    :: Word64
  , dibtEncoding :: DwarfAttrEncoding
  , dibtFlags    :: Maybe DIFlags
  }
  deriving (Data, Eq, Generic, Ord, Show)

data DICompileUnit = DICompileUnit
  { dicuLanguage           :: DwarfLang
  , dicuFile               :: Maybe ValMd
  , dicuProducer           :: Maybe String
  , dicuIsOptimized        :: Bool
  , dicuFlags              :: Maybe String
  , dicuRuntimeVersion     :: Word16
  , dicuSplitDebugFilename :: Maybe FilePath
  , dicuEmissionKind       :: DIEmissionKind
  , dicuEnums              :: Maybe ValMd
  , dicuRetainedTypes      :: Maybe ValMd
  , dicuSubprograms        :: Maybe ValMd
  , dicuGlobals            :: Maybe ValMd
  , dicuImports            :: Maybe ValMd
  , dicuMacros             :: Maybe ValMd
  , dicuDWOId              :: Word64
  , dicuSplitDebugInlining :: Bool
  , dicuDebugInfoForProf   :: Bool
  , dicuNameTableKind      :: Word64
  , dicuRangesBaseAddress  :: Word64
  , dicuSysRoot            :: Maybe String
  , dicuSDK                :: Maybe String
  }
  deriving (Data, Eq, Generic, Ord, Show)

data DICompositeType = DICompositeType
  { dictTag            :: DwarfTag
  , dictName           :: Maybe String
  , dictFile           :: Maybe ValMd
  , dictLine           :: Word32
  , dictScope          :: Maybe ValMd
  , dictBaseType       :: Maybe ValMd
  , dictSize           :: Word64
  , dictAlign          :: Word64
  , dictOffset         :: Word64
  , dictFlags          :: DIFlags
  , dictElements       :: Maybe ValMd
  , dictRuntimeLang    :: DwarfLang
  , dictVTableHolder   :: Maybe ValMd
  , dictTemplateParams :: Maybe ValMd
  , dictIdentifier     :: Maybe String
  , dictDiscriminator  :: Maybe ValMd
  , dictDataLocation   :: Maybe ValMd
  }
  deriving (Data, Eq, Generic, Ord, Show)

data DIDerivedType = DIDerivedType
  { didtTag       :: DwarfTag
  , didtName      :: Maybe String
  , didtFile      :: Maybe ValMd
  , didtLine      :: Word32
  , didtScope     :: Maybe ValMd
  , didtBaseType  :: Maybe ValMd
  , didtSize      :: Word64
  , didtAlign     :: Word64
  , didtOffset    :: Word64
  , didtFlags     :: DIFlags
  , didtExtraData :: Maybe ValMd
  }
  deriving (Data, Eq, Generic, Ord, Show)

newtype DIExpression = DIExpression
  { dieElements :: [Word64]
  }
  deriving (Data, Eq, Generic, Ord, Show)

data DIFile = DIFile
  { difFilename  :: FilePath
  , difDirectory :: FilePath
  }
  deriving (Data, Eq, Generic, Ord, Show)

data DIGlobalVariable = DIGlobalVariable
  { digvScope        :: Maybe ValMd
  , digvName         :: Maybe String
  , digvLinkageName  :: Maybe String
  , digvFile         :: Maybe ValMd
  , digvLine         :: Word32
  , digvType         :: Maybe ValMd
  , digvIsLocal      :: Bool
  , digvIsDefinition :: Bool
  , digvVariable     :: Maybe ValMd
  , digvDeclaration  :: Maybe ValMd
  , digvAlignment    :: Maybe Word32
  }
  deriving (Data, Eq, Generic, Ord, Show)

data DIGlobalVariableExpression = DIGlobalVariableExpression
  { digveVariable   :: Maybe ValMd
  , digveExpression :: Maybe ValMd
  }
  deriving (Data, Eq, Generic, Ord, Show)

data DILexicalBlock = DILexicalBlock
  { dilbScope  :: Maybe ValMd
  , dilbFile   :: Maybe ValMd
  , dilbLine   :: Word32
  , dilbColumn :: Word16
  }
  deriving (Data, Eq, Generic, Ord, Show)

data DILexicalBlockFile = DILexicalBlockFile
  { dilbfScope         :: ValMd
  , dilbfFile          :: Maybe ValMd
  , dilbfDiscriminator :: Word32
  }
  deriving (Data, Eq, Generic, Ord, Show)

data DILocalVariable = DILocalVariable
  { dilvScope :: Maybe ValMd
  , dilvName  :: Maybe String
  , dilvFile  :: Maybe ValMd
  , dilvLine  :: Word32
  , dilvType  :: Maybe ValMd
  , dilvArg   :: Word16
  , dilvFlags :: DIFlags
  }
  deriving (Data, Eq, Generic, Ord, Show)

data DISubprogram = DISubprogram
  { dispScope          :: Maybe ValMd
  , dispName           :: Maybe String
  , dispLinkageName    :: Maybe String
  , dispFile           :: Maybe ValMd
  , dispLine           :: Word32
  , dispType           :: Maybe ValMd
  , dispIsLocal        :: Bool
  , dispIsDefinition   :: Bool
  , dispScopeLine      :: Word32
  , dispContainingType :: Maybe ValMd
  , dispVirtuality     :: DwarfVirtuality
  , dispVirtualIndex   :: Word32
  , dispThisAdjustment :: Int64
  , dispFlags          :: DIFlags
  , dispIsOptimized    :: Bool
  , dispUnit           :: Maybe ValMd
  , dispTemplateParams :: Maybe ValMd
  , dispDeclaration    :: Maybe ValMd
  , dispVariables      :: Maybe ValMd
  , dispThrownTypes    :: Maybe ValMd
  }
  deriving (Data, Eq, Generic, Ord, Show)

data DISubrange = DISubrange
  { disrCount      :: Int64
  , disrLowerBound :: Int64
  }
  deriving (Data, Eq, Generic, Ord, Show)

data DISubroutineType = DISubroutineType
  { distFlags     :: DIFlags
  , distTypeArray :: Maybe ValMd
  }
  deriving (Data, Eq, Generic, Ord, Show)

type UniqueId = Int

-- data Unique a = Unique { uniqueId :: UniqueId,  uniqueValue :: a }
--   deriving (Eq, Ord, Data, Generic, Show)

-- type Instr = Unique Instr'

-- Resolve identifiers, labels and symbols
data Instr
  = Ret Value
    {- ^ * Return from function with the given value.
         * Ends basic block. -}

  | RetVoid
    {- ^ * Return from function.
         * Ends basic block. -}

  | Arith ArithOp Value Value
    {- ^ * Binary arithmetic operation, both operands have the same type.
         * Middle of basic block.
         * The result is the same as parameters. -}

  | Bit BitOp Value Value
    {- ^ * Binary bit-vector operation, both operands have the same type.
         * Middle of basic block.
         * The result is the same as parameters. -}

  | Conv ConvOp Value Type
    {- ^ * Convert a value from one type to another.
         * Middle of basic block.
         * The result matches the 3rd parameter. -}

  | Call Bool Type Value [Value]
    {- ^ * Call a function.
            The boolean is tail-call hint (XXX: needs to be updated)
            The type is always a FunTy
         * Middle of basic block.
         * The result is as indicated by the provided type. -}

  | Alloca Type (Maybe Value) (Maybe Int)
    {- ^ * Allocated space on the stack:
           type of elements;
           how many elements (1 if 'Nothing');
           required alignment.
         * Middle of basic block.
         * Returns a pointer to hold the given number of elements. -}

  | Load Value (Maybe AtomicOrdering) (Maybe Align)
    {- ^ * Read a value from the given address:
           address to read from;
           atomic ordering;
           assumptions about alignment of the given pointer.
         * Middle of basic block.
         * Returns a value of type matching the pointer. -}

  | Store Value Value (Maybe AtomicOrdering) (Maybe Align)
    {- ^ * Write a value to memory:
             value to store;
             pointer to location where to store;
             atomic ordering;
             assumptions about the alignment of the given pointer.
         * Middle of basic block.
         * Effect. -}


  | Fence (Maybe String) AtomicOrdering
    {- ^ * Introduce a happens-before relationship between operations:
             synchronization scope;
             type of ordering.
         * Middle of basic block. -}

  | CmpXchg Bool Bool Value Value Value (Maybe String) AtomicOrdering AtomicOrdering
    {- ^ * Atomically compare and maybe exchange values in memory:
             whether the exchange is weak;
             whether the exchange is volatile;
             pointer to read;
             value to compare it with;
             new value to write if the two prior values are equal;
             synchronization scope;
             synchronization ordering on success;
             synchronization ordering on failure.
         * Returns a pair of the original value and whether an exchange occurred.
         * Middle of basic block.
         * Effect. -}

  | AtomicRW Bool AtomicRWOp Value Value (Maybe String) AtomicOrdering
    {- ^ * Perform an atomic load, operation, and store:
             whether the operation is volatile;
             operation to apply to the read value and the provided value;
             pointer to read;
             value to combine it with, using the given operation;
             synchronization scope;
             synchronization ordering.
         * Returns the original value at the given location.
         * Middle of basic block.
         * Effect. -}

  | ICmp ICmpOp Value Value
    {- ^ * Compare two integral values.
         * Middle of basic block.
         * Returns a boolean value. -}

  | FCmp FCmpOp Value Value
    {- ^ * Compare two floating point values.
         * Middle of basic block.
         * Returns a boolean value. -}

  | Phi Type [(Value,BasicBlock)]
    {- ^ * Join point for an SSA value: we get one value per predecessor
           basic block.
         * Middle of basic block.
         * Returns a value of the specified type. -}

  | GEP Bool Value [Value]
    {- ^ * "Get element pointer",
            compute the address of a field in a structure:
            inbounds check (value poisoned if this fails);
            pointer to parent structure;
            path to a sub-component of a structure.
         * Middle of basic block.
         * Returns the address of the requested member.

    The types in path are the types of the index, not the fields.

    The indexes are in units of fields (i.e., the first element in
    a struct is field 0, the next one is 1, etc., regardless of the size
    of the fields in bytes). -}

  | Select Value Value Value
    {- ^ * Local if-then-else; the first argument is boolean, if
           true pick the 2nd argument, otherwise evaluate to the 3rd.
         * Middle of basic block.
         * Returns either the 2nd or the 3rd argument. -}

  | ExtractValue Value [Int32]
    {- ^ * Get the value of a member of an aggregate value:
           the first argument is an aggregate value (not a pointer!),
           the second is a path of indexes, similar to the one in 'GEP'.
         * Middle of basic block.
         * Returns the given member of the aggregate value. -}

  | InsertValue Value Value [Int32]
    {- ^ * Set the value for a member of an aggregate value:
           the first argument is the value to insert, the second is the
           aggreagate value to be modified.
         * Middle of basic block.
         * Returns an updated aggregate value. -}

  | ExtractElt Value Value
    {- ^ * Get an element from a vector: the first argument is a vector,
           the second an index.
         * Middle of basic block.
         * Returns the element at the given position. -}

  | InsertElt Value Value Value
    {- ^ * Modify an element of a vector: the first argument is the vector,
           the second the value to be inserted, the third is the index where
           to insert the value.
         * Middle of basic block.
         * Returns an updated vector. -}


  | ShuffleVector Value Value Value


  | Jump BasicBlock
    {- ^ * Jump to the given basic block.
         * Ends basic block. -}

  | Br Value BasicBlock BasicBlock
    {- ^ * Conditional jump: if the value is true jump to the first basic
           block, otherwise jump to the second.
         * Ends basic block. -}

  | Invoke Type Value [Value] BasicBlock BasicBlock
    -- ^ normal label, unwind label
    -- The type is always a FunTy

  | Comment String
    -- ^ Comment

  | Unreachable
    -- ^ No defined sematics, we should not get to here.

  | Unwind
  | VaArg Value Type
  | IndirectBr Value [BasicBlock]

  | Switch Value BasicBlock [(Integer,BasicBlock)]
    {- ^ * Multi-way branch: the first value determines the direction
           of the branch, the label is a default direction, if the value
           does not appear in the jump table, the last argument is the
           jump table.
         * Ends basic block. -}

  | LandingPad Type (Maybe Value) Bool [Clause]

  | Resume Value

    deriving (Data, Eq, Generic, Ord, Show)

--

data GlobalAlias = GlobalAlias
  { aliasName  :: Symbol
  , aliasType  :: Type
  , aliasValue :: Value
  , aliasUniqueId :: UniqueId
  }
  deriving (Data, Generic, Eq, Ord, Show)

data Global = Global
  { globalSym      :: Symbol
  , globalAttrs    :: GlobalAttrs
  , globalType     :: Type
  , globalValue    :: Maybe Value
  , globalUniqueId :: UniqueId
  , globalAlign    :: Maybe Align
  , globalMetadata :: GlobalMdAttachments
  }
  deriving (Data, Generic, Eq, Ord, Show)

type GlobalMdAttachments = Map KindMd ValMd

data Declare = Declare
  { decRetType  :: Type
  , decName     :: Symbol
  , decArgs     :: [Type]
  , decVarArgs  :: Bool
  , decAttrs    :: [FunAttr]
  , decUniqueId :: UniqueId
  , decComdat   :: Maybe String
  }
  deriving (Data, Generic, Eq, Ord, Show)

instance Hashable Declare where
  hashWithSalt s = hashWithSalt s . decUniqueId

data Define = Define
  { defLinkage  :: Maybe Linkage
  , defRetType  :: Type
  , defName     :: Symbol
  , defArgs     :: [Argument]
  , defVarArgs  :: Bool
  , defAttrs    :: [FunAttr]
  , defSection  :: Maybe String
  , defGC       :: Maybe GC
  , defBody     :: [BasicBlock]
  , defUniqueId :: UniqueId
  , defMetadata :: FnMdAttachments
  , defComdat   :: Maybe String
  }
  deriving (Data, Generic, Eq, Ord, Show)

instance Hashable Define where
  hashWithSalt s = hashWithSalt s . defUniqueId

type FnMdAttachments = Map KindMd ValMd

data Module = Module
  { modSourceName :: Maybe String
  , modDataLayout :: DataLayout
  , modTypes      :: [TypeDecl]
  , modNamedMd    :: [NamedMd]
  , modUnnamedMd  :: [UnnamedMd]
  , modComdat     :: Map String SelectionKind
  , modGlobals    :: [Global]
  , modDeclares   :: [Declare]
  , modDefines    :: [Define]
  , modInlineAsm  :: InlineAsm
  , modAliases    :: [GlobalAlias]
  }
  deriving (Data, Generic, Eq, Ord, Show)

data TypeDecl
  = TypeDecl
    { typeName  :: Ident
    , typeValue :: Type
    } deriving (Data, Generic, Eq, Ord, Show)

-- FIXME: missing parameter attributes
data Argument = Argument
  { argDefine :: Define
  , argName   :: Ident
  , argType   :: Type
  , argUniqueId :: UniqueId
  }
  deriving (Data, Generic, Eq, Ord, Show)

instance Hashable Argument where
  hashWithSalt s = hashWithSalt s . argUniqueId

{-# INLINABLE stripBitcasts #-}
-- | Strip all wrapper bitcasts from a Value
stripBitcasts :: Value -> Value
stripBitcasts (Value _ _ (ValIdent (IdentValStmt Stmt {stmtInstr = Conv BitCast cv _}))) = stripBitcasts cv
stripBitcasts (Value _ _ (ValConstExpr (ConstConv BitCast cv _))) = stripBitcasts cv
stripBitcasts v = v
