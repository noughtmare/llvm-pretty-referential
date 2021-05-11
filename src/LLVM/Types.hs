{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE ViewPatterns, LambdaCase, StrictData #-}
{-# OPTIONS_GHC -O0 #-}
module LLVM.Types
  ( module Text.LLVM.AST
  , UniqueId
  -- , Typed (..)
  , SymValue(..)
  , IdentValue(..)
  , Value(..)
  , Value'(..)
  , Type(..)
  , BasicBlock(..)
  , StmtType(..)
  , Stmt(..)
  , Clause(..)
  , ConstExpr(..)
  , Instr(..)
  , TypeDecl(..)
  , Global(..)
  , Define(..)
  , Declare(..)
  , GlobalAlias(..)
  , Module(..)
  , UnnamedMd(..)
  , Argument(..)
  , ValMd(..)
  , DebugLoc(..)
  , DebugInfo(..)
  , DIFile(..)
  , DILabel(..)
  , DIFlags
  , DwarfTag(..)
  , DwarfLang
  , DwarfVirtuality
  , DwarfAttrEncoding(..)
  , DISubrange(..)
  , DIBasicType(..)
  , DIExpression(..)
  , DINameSpace(..)
  , DISubprogram(..)
  , DICompileUnit(..)
  , DIDerivedType(..)
  , DILexicalBlock(..)
  , DIEmissionKind
  , DICompositeType(..)
  , DILocalVariable(..)
  , DIGlobalVariable(..)
  , DISubroutineType(..)
  , DIImportedEntity(..)
  , DILexicalBlockFile(..)
  , DIGlobalVariableExpression(..)
  , DITemplateTypeParameter(..)
  , DITemplateValueParameter(..)
  , HasUniqueId(..)
  , prettyType
  ) where

import           Text.LLVM.AST                  ( Align
                                                , ArithOp(..)
                                                , AtomicOrdering(..)
                                                , AtomicRWOp(..)
                                                , BitOp(..)
                                                , BlockLabel(..)
                                                , ConvOp(..)
                                                , DataLayout
                                                , FCmpOp(..)
                                                , FP80Value(..)
                                                , FloatType(..)
                                                , FunAttr(..)
                                                , GC(..)
                                                , GlobalAttrs(..)
                                                , ICmpOp(..)
                                                , Ident(..)
                                                , InlineAsm
                                                , KindMd
                                                , LayoutSpec(..)
                                                , Linkage(..)
                                                , NamedMd(..)
                                                , PrimType(..)
                                                , SelectionKind(..)
                                                , Symbol(..)
                                                , Visibility(..)
                                                )

import           Data.Map.Strict                ( Map )

import           Control.DeepSeq                ( NFData(rnf) )
import           Data.Data                      ( Data )
import           Data.Function                  ( on )
import           Data.Hashable                  ( Hashable(hashWithSalt) )
import           Data.Int                       ( Int32
                                                , Int64
                                                )
import           Data.Word                      ( Word16
                                                , Word32
                                                , Word64
                                                , Word8
                                                )
import           GHC.Generics                   ( Generic )

import           Data.Char                      ( chr )
import           Data.List                      ( intercalate )

class HasUniqueId a where
  uniqueId :: a -> UniqueId

-- TODO this is probably no longer true
--
-- This isn't very honest, but Values are part of Modules and
-- are fully evaluated before the module is constructed.
instance NFData Stmt where
  rnf _ = ()
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
--   deriving (Eq, Ord, Generic, Show)

data SymValue
  = SymValAlias GlobalAlias
  | SymValGlobal Global
  | SymValDeclare Declare
  | SymValDefine Define
  deriving (Generic, Eq, Ord)

instance Show SymValue where
  show (SymValAlias   x) = show x
  show (SymValGlobal  x) = show x
  show (SymValDeclare x) = show x
  show (SymValDefine  x) = show x

instance HasUniqueId SymValue where
  uniqueId (SymValAlias   ga ) = uniqueId ga
  uniqueId (SymValGlobal  g  ) = uniqueId g
  uniqueId (SymValDeclare dec) = uniqueId dec
  uniqueId (SymValDefine  def) = uniqueId def

data IdentValue
  = IdentValArgument Argument
  | IdentValStmt Stmt
  deriving (Generic, Eq, Ord)

instance Show IdentValue where
  show (IdentValArgument x) = show x
  show (IdentValStmt     x) = show x

instance HasUniqueId IdentValue where
  uniqueId (IdentValArgument arg ) = uniqueId arg
  uniqueId (IdentValStmt     stmt) = uniqueId stmt

data Value = Value
  { valType     :: Type
  , valUniqueId :: UniqueId
  , valValue    :: Value'
  , valName     :: Maybe String
  }
  deriving (Generic, Show)

instance HasUniqueId Value where
  uniqueId = valUniqueId

instance Eq Value where
  (==) = (==) `on` uniqueId

instance Ord Value where
  compare x y = {- trace ("COMPARE " ++ show (valValue x) ++ " AND " ++ show (valValue y)) $ -}(compare `on` uniqueId) x y

instance Hashable Value where
  hashWithSalt s = hashWithSalt s . uniqueId

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
  deriving (Generic)

instance Show Value' where
  show (ValInteger x    ) = show x
  show (ValBool    True ) = "true"
  show (ValBool    False) = "false"
  show (ValFloat   x    ) = show x
  show (ValDouble  x    ) = show x
  show (ValFP80    x    ) = show x
  show (ValIdent   x    ) = show x
  show (ValSymbol  x    ) = show x
  show ValNull            = "NULL"
  show (ValArray  _  xs)  = "[ " ++ intercalate ", " (map show xs) ++ " ]"
  show (ValVector _  xs)  = "< " ++ unwords (map show xs) ++ " >"
  show (ValStruct xs _ )  = "{ " ++ intercalate ", " (map show xs) ++ " }"
  show (ValString    xs)  = "c" ++ show (map (chr . fromIntegral) xs)
  show (ValConstExpr x )  = show x
  show ValUndef           = "undef"
  show (ValLabel x)       = show x
  show ValZeroInit        = "zeroinitializer"
  show (ValAsm _ _ x y)   = "asm " ++ x ++ ", " ++ y
  show (ValMd _       )   = "<metadata>"

-- Resolve type names
data Type
  = PrimType PrimType
  | Array Word64 Type
  | FunTy Type [Type] Bool
  | PtrTo Type
  | Struct (Either UniqueId Ident) ~[Type] Bool
  | Vector Word64 Type
  | Opaque (Either UniqueId Ident)
  deriving (Generic)

instance Show Type where
  show = prettyType

prettyType :: Type -> String
prettyType = \case
  (PrimType x           ) -> prettyPrimType x
  (Array n x            ) -> concat ["[", show n, " x ", prettyType x, "]"]
  (FunTy x xs hasVarArgs) -> concat
    [ prettyType x
    , " ("
    , intercalate ", " (map prettyType xs)
    , if hasVarArgs then ", ..." else ""
    , ")"
    ]
  (PtrTo x             ) -> prettyType x <> "*"
  (Struct (Left  i) _ _) -> "%struct.anon" <> show i
  (Struct (Right (Ident i)) _ _) -> "%" <> i
  (Vector n x          ) -> concat ["<", show n, " x ", prettyType x, ">"]
  (Opaque _            ) -> "opaque"

prettyPrimType :: PrimType -> String
prettyPrimType = \case
  Label -> "label"
  Void -> "void"
  (Integer i) -> "i" <> show i
  (FloatType f) -> prettyFloatType f
  X86mmx -> "x86_mmx"
  Metadata -> "metadata"

prettyFloatType :: FloatType -> String
prettyFloatType = \case
  Half -> "half"
  Float -> "float"
  Double -> "double"
  Fp128 -> "fp128"
  X86_fp80 -> "x86_fp80"
  PPC_fp128 -> "ppc_fp128"

instance Eq Type where
  PrimType t1 == PrimType t2 = t1 == t2
  Array i1 t1 == Array i2 t2 = i1 == i2 && t1 == t2
  FunTy t1 ts1 b1 == FunTy t2 ts2 b2 = b1 == b2 && t1 == t2 && ts1 == ts2
  PtrTo t1 == PtrTo t2 = t1 == t2
  Struct x _ _ == Struct y _ _ = x == y
  Vector i1 t1 == Vector i2 t2 = i1 == i2 && t1 == t2
  Opaque x == Opaque y = x == y
  _ == _ = False

instance Ord Type where
  compare (PrimType t1) (PrimType t2) = compare t1 t2
  compare PrimType{} _ = LT
  compare _ PrimType{} = GT

  compare (Array i1 t1) (Array i2 t2) = compare i1 i2 <> compare t1 t2
  compare Array{} _ = LT
  compare _ Array{} = GT

  compare (FunTy t1 ts1 b1) (FunTy t2 ts2 b2) = compare b1 b2 <> compare t1 t2 <> compare ts1 ts2
  compare FunTy{} _ = LT
  compare _ FunTy{} = GT

  compare (PtrTo t1) (PtrTo t2) = compare t1 t2
  compare PtrTo{} _ = LT
  compare _ PtrTo{} = GT

  compare (Struct x _ _) (Struct y _ _) = compare x y
  compare Struct{} _ = LT
  compare _ Struct{} = GT

  compare (Vector i1 t1) (Vector i2 t2) = compare i1 i2 <> compare t1 t2
  compare Vector{} _ = LT
  compare _ Vector{} = GT

  compare (Opaque x) (Opaque y) = compare x y
  -- compare Opaque{} _ = LT
  -- compare _ Opaque{} = GT

instance Hashable Ident where
  hashWithSalt s (Ident i) = hashWithSalt s i

instance Hashable FloatType where
  hashWithSalt s = hashWithSalt s . fromEnum

instance Hashable PrimType where
  hashWithSalt s Label         = s `hashWithSalt` (1 :: Int)
  hashWithSalt s Void          = s `hashWithSalt` (2 :: Int)
  hashWithSalt s (Integer   i) = s `hashWithSalt` i `hashWithSalt` (3 :: Int)
  hashWithSalt s (FloatType f) = s `hashWithSalt` f `hashWithSalt` (4 :: Int)
  hashWithSalt s X86mmx        = s `hashWithSalt` (5 :: Int)
  hashWithSalt s Metadata      = s `hashWithSalt` (6 :: Int)

instance Hashable Type where
  hashWithSalt s (PrimType p) = s `hashWithSalt` p `hashWithSalt` (1 :: Int)
  hashWithSalt s (Array i t) =
    s `hashWithSalt` i `hashWithSalt` t `hashWithSalt` (2 :: Int)
  hashWithSalt s (FunTy t ts b) =
    s
      `hashWithSalt` t
      `hashWithSalt` ts
      `hashWithSalt` b
      `hashWithSalt` (3 :: Int)
  hashWithSalt s (PtrTo t) = s `hashWithSalt` t `hashWithSalt` (4 :: Int)
  hashWithSalt s (Struct (Right n) _ _) =
    s `hashWithSalt` n `hashWithSalt` (5 :: Int)
  hashWithSalt s (Struct (Left i) _ p) =
    s `hashWithSalt` i `hashWithSalt` p `hashWithSalt` (5 :: Int)
  hashWithSalt s (Vector n t) =
    s `hashWithSalt` n `hashWithSalt` t `hashWithSalt` (6 :: Int)
  hashWithSalt s (Opaque n) = s `hashWithSalt` n `hashWithSalt` (7 :: Int)

data BasicBlock = BasicBlock
  { bbName     :: String
  , bbLabel    :: Maybe BlockLabel
  , bbStmts    :: [Stmt]
  , bbUniqueId :: UniqueId
  , bbDefine   :: ~Define
  }
  deriving (Generic)

instance Show BasicBlock where
  show BasicBlock { bbName = x } = x

instance HasUniqueId BasicBlock where
  uniqueId = bbUniqueId

instance Eq BasicBlock where
  (==) = (==) `on` uniqueId
instance Ord BasicBlock where
  compare = compare `on` uniqueId
instance Hashable BasicBlock where
  hashWithSalt s = hashWithSalt s . uniqueId

-- instance Show BasicBlock where
--   show = ("bb" ++ ) . show . bbUniqueId

data StmtType = Result Ident | Effect deriving (Eq, Ord, Generic, Show)

data Stmt = Stmt
  { stmtType       :: StmtType
  , stmtInstr      :: Instr
  , stmtMd         :: [(String, ValMd)]
  , stmtUniqueId   :: UniqueId
  , stmtBasicBlock :: ~BasicBlock
  }
  deriving (Generic)

instance HasUniqueId Stmt where
  uniqueId = stmtUniqueId

instance Eq       Stmt where
  (==) = (==) `on` uniqueId
instance Ord      Stmt where
  compare = compare `on` uniqueId
instance Hashable Stmt where
  hashWithSalt s = hashWithSalt s . uniqueId

instance Show Stmt where
  show = ("stmt" ++) . show . stmtUniqueId

data Clause
  = Catch Value
  | Filter Value
  deriving (Eq, Ord, Generic, Show)

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
  deriving (Eq, Generic, Ord, Show)

data ValMd
  = ValMdString String
  | ValMdValue Value
  | ValMdNode [Maybe ValMd]
  | ValMdLoc DebugLoc
  | ValMdDebugInfo DebugInfo
  deriving (Generic)

data DebugLoc = DebugLoc
  { dlLine     :: Word32
  , dlCol      :: Word32
  , dlScope    :: ~ValMd
  , dlIA       :: Maybe ValMd
  , dlImplicit :: Bool
  }
  deriving (Generic)

-- instance Eq DebugLoc where
--   DebugLoc x1 x2 _ x4 x5 == DebugLoc y1 y2 _ y4 y5 = x1 == y1 && x2 == y2 && x5 == y5 && x4 == y4
-- 
-- instance Ord DebugLoc where
--   compare (DebugLoc x1 x2 _ x4 x5

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
  deriving (Generic)

data DILabel = DILabel
  { dilScope :: ~(Maybe ValMd)
  , dilName  :: String
  , dilFile  :: Maybe ValMd
  , dilLine  :: Word32
  }
  deriving (Generic)

data DIImportedEntity = DIImportedEntity
  { diieTag    :: DwarfTag
  , diieScope  :: ~(Maybe ValMd)
  , diieEntity :: Maybe ValMd
  , diieFile   :: Maybe ValMd
  , diieLine   :: Word32
  , diieName   :: Maybe String
  }
  deriving (Generic)

data DITemplateTypeParameter = DITemplateTypeParameter
  { dittpName :: Maybe String
  , dittpType :: Maybe ValMd
  }
  deriving (Generic)

data DITemplateValueParameter = DITemplateValueParameter
  { ditvpTag   :: DwarfTag
  , ditvpName  :: Maybe String
  , ditvpType  :: Maybe ValMd
  , ditvpValue :: ValMd
  }
  deriving (Generic)

data DINameSpace = DINameSpace
  { dinsName  :: Maybe String
  , dinsScope :: ~ValMd
  , dinsFile  :: ValMd
  , dinsLine  :: Word32
  }
  deriving (Generic)

-- TODO: Turn these into sum types
-- See https://github.com/llvm-mirror/llvm/blob/release_38/include/llvm/Support/Dwarf.def
type DwarfLang = Word16
-- type DwarfTag = Word16
type DwarfVirtuality = Word8
-- See https://github.com/llvm-mirror/llvm/blob/release_38/include/llvm/IR/DebugInfoMetadata.h#L175
type DIFlags = Word32
-- This seems to be defined internally as a small enum, and defined
-- differently across versions. Maybe turn this into a sum type once
-- it stabilizes.
type DIEmissionKind = Word8

data DwarfAttrEncoding
  = DW_ATE_address
  | DW_ATE_boolean
  | DW_ATE_complex_float
  | DW_ATE_float
  | DW_ATE_signed
  | DW_ATE_signed_char
  | DW_ATE_unsigned
  | DW_ATE_unsigned_char
  | DW_ATE_imaginary_float
  | DW_ATE_packed_decimal
  | DW_ATE_numeric_string
  | DW_ATE_edited
  | DW_ATE_signed_fixed
  | DW_ATE_unsigned_fixed
  | DW_ATE_decimal_float
  | DW_ATE_UTF
  deriving (Eq, Ord, Enum, Read, Show, Generic)

data DwarfTag
 = DW_TAG_array_type
 | DW_TAG_class_type
 | DW_TAG_entry_point
 | DW_TAG_enumeration_type
 | DW_TAG_formal_parameter
 | DW_TAG_imported_declaration
 | DW_TAG_label
 | DW_TAG_lexical_block
 | DW_TAG_member
 | DW_TAG_pointer_type
 | DW_TAG_reference_type
 | DW_TAG_compile_unit
 | DW_TAG_string_type
 | DW_TAG_structure_type
 | DW_TAG_subroutine_type
 | DW_TAG_typedef
 | DW_TAG_union_type
 | DW_TAG_unspecified_parameters
 | DW_TAG_variant
 | DW_TAG_common_block
 | DW_TAG_common_inclusion
 | DW_TAG_inheritance
 | DW_TAG_inlined_subroutine
 | DW_TAG_module
 | DW_TAG_ptr_to_member_type
 | DW_TAG_set_type
 | DW_TAG_subrange_type
 | DW_TAG_with_stmt
 | DW_TAG_access_declaration
 | DW_TAG_base_type
 | DW_TAG_catch_block
 | DW_TAG_const_type
 | DW_TAG_constant
 | DW_TAG_enumerator
 | DW_TAG_file_type
 | DW_TAG_friend
 | DW_TAG_namelist
 | DW_TAG_namelist_item
 | DW_TAG_packed_type
 | DW_TAG_subprogram
 | DW_TAG_template_type_parameter
 | DW_TAG_template_value_parameter
 | DW_TAG_thrown_type
 | DW_TAG_try_block
 | DW_TAG_variant_part
 | DW_TAG_variable
 | DW_TAG_volatile_type
 | DW_TAG_dwarf_procedure
 | DW_TAG_restrict_type
 | DW_TAG_interface_type
 | DW_TAG_namespace
 | DW_TAG_imported_module
 | DW_TAG_unspecified_type
 | DW_TAG_partial_unit
 | DW_TAG_imported_unit
 | DW_TAG_condition
 | DW_TAG_shared_type
 | DW_TAG_type_unit
 | DW_TAG_rvalue_reference_type
 | DW_TAG_template_alias
 deriving (Eq, Ord, Enum, Show, Read, Generic)

data DIBasicType = DIBasicType
  { dibtTag      :: DwarfTag
  , dibtName     :: String
  , dibtSize     :: Word64
  , dibtAlign    :: Word64
  , dibtEncoding :: DwarfAttrEncoding
  , dibtFlags    :: Maybe DIFlags
  }
  deriving (Eq, Generic, Ord, Show)

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
  deriving (Generic)

data DICompositeType = DICompositeType
  { dictTag            :: DwarfTag
  , dictName           :: Maybe String
  , dictFile           :: Maybe ValMd
  , dictLine           :: Word32
  , dictScope          :: ~(Maybe ValMd)
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
  deriving (Generic)

data DIDerivedType = DIDerivedType
  { didtTag       :: DwarfTag
  , didtName      :: Maybe String
  , didtFile      :: Maybe ValMd
  , didtLine      :: Word32
  , didtScope     :: ~(Maybe ValMd)
  , didtBaseType  :: Maybe ValMd
  , didtSize      :: Word64
  , didtAlign     :: Word64
  , didtOffset    :: Word64
  , didtFlags     :: DIFlags
  , didtExtraData :: Maybe ValMd
  }
  deriving (Generic)

newtype DIExpression = DIExpression
  { dieElements :: [Word64]
  }
  deriving (Eq, Generic, Ord, Show)

data DIFile = DIFile
  { difFilename  :: FilePath
  , difDirectory :: FilePath
  }
  deriving (Eq, Generic, Ord, Show)

data DIGlobalVariable = DIGlobalVariable
  { digvScope        :: ~(Maybe ValMd)
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
  deriving (Generic)

data DIGlobalVariableExpression = DIGlobalVariableExpression
  { digveVariable   :: Maybe ValMd
  , digveExpression :: Maybe ValMd
  }
  deriving (Generic)

data DILexicalBlock = DILexicalBlock
  { dilbScope  :: ~(Maybe ValMd)
  , dilbFile   :: Maybe ValMd
  , dilbLine   :: Word32
  , dilbColumn :: Word16
  }
  deriving (Generic)

data DILexicalBlockFile = DILexicalBlockFile
  { dilbfScope         :: ~ValMd
  , dilbfFile          :: Maybe ValMd
  , dilbfDiscriminator :: Word32
  }
  deriving (Generic)

data DILocalVariable = DILocalVariable
  { dilvScope :: ~(Maybe ValMd)
  , dilvName  :: Maybe String
  , dilvFile  :: Maybe ValMd
  , dilvLine  :: Word32
  , dilvType  :: Maybe ValMd
  , dilvArg   :: Word16
  , dilvFlags :: DIFlags
  }
  deriving (Generic)

data DISubprogram = DISubprogram
  { dispScope          :: ~(Maybe ValMd)
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
  deriving (Generic)

data DISubrange = DISubrange
  { disrCount      :: Int64
  , disrLowerBound :: Int64
  }
  deriving (Eq, Generic, Ord, Show)

data DISubroutineType = DISubroutineType
  { distFlags     :: DIFlags
  , distTypeArray :: Maybe ValMd
  }
  deriving (Generic)

type UniqueId = Int

-- data Unique a = Unique { uniqueId :: UniqueId,  uniqueValue :: a }
--   deriving (Eq, Ord, Generic, Show)

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

  | Phi Type [(Value, BasicBlock)]
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

  | Switch Value BasicBlock [(Integer, BasicBlock)]
    {- ^ * Multi-way branch: the first value determines the direction
           of the branch, the label is a default direction, if the value
           does not appear in the jump table, the last argument is the
           jump table.
         * Ends basic block. -}

  | LandingPad Type (Maybe Value) Bool [Clause]

  | Resume Value

    deriving (Eq, Generic, Ord, Show)

--

data GlobalAlias = GlobalAlias
  { aliasLinkage    :: Maybe Linkage
  , aliasVisibility :: Maybe Visibility
  , aliasName       :: Symbol
  , aliasType       :: Type
  , aliasTarget     :: Value
  , aliasUniqueId   :: UniqueId
  }
  deriving (Generic)

instance Show GlobalAlias where
  show GlobalAlias { aliasName = Symbol x } = "@" ++ x

instance HasUniqueId GlobalAlias where
  uniqueId = aliasUniqueId

instance Eq       GlobalAlias where
  (==) = (==) `on` uniqueId
instance Ord      GlobalAlias where
  compare = compare `on` uniqueId
instance Hashable GlobalAlias where
  hashWithSalt s = hashWithSalt s . uniqueId

-- instance Show GlobalAlias where
--   show = ("ga" ++ ) . show . aliasUniqueId

data Global = Global
  { globalName     :: Symbol
  , globalAttrs    :: GlobalAttrs
  , globalType     :: Type
  , globalValue    :: Maybe Value
  , globalUniqueId :: UniqueId
  , globalAlign    :: Maybe Align
  , globalMetadata :: GlobalMdAttachments
  }
  deriving (Generic)

instance Show Global where
  show (Global name attrs ty val uniqueid align _meta) = unwords ["Global", show name, show attrs, show ty, show val, show uniqueid, show align, "<meta>"]

instance HasUniqueId Global where
  uniqueId = globalUniqueId

instance Eq       Global where
  (==) = (==) `on` uniqueId
instance Ord      Global where
  compare = compare `on` uniqueId
instance Hashable Global where
  hashWithSalt s = hashWithSalt s . uniqueId

-- instance Show Global where
--   show = ("g" ++ ) . show . globalUniqueId

type GlobalMdAttachments = Map KindMd ValMd

data Declare = Declare
  { decLinkage    :: Maybe Linkage
  , decVisibility :: Maybe Visibility
  , decRetType    :: Type
  , decName       :: Symbol
  , decArgs       :: [Type]
  , decVarArgs    :: Bool
  , decAttrs      :: [FunAttr]
  , decUniqueId   :: UniqueId
  , decComdat     :: Maybe String
  }
  deriving (Generic)

instance Show Declare where
  show Declare { decName = Symbol x } = "@" ++ x

instance HasUniqueId Declare where
  uniqueId = decUniqueId

instance Eq       Declare where
  (==) = (==) `on` uniqueId
instance Ord      Declare where
  compare = compare `on` uniqueId
instance Hashable Declare where
  hashWithSalt s = hashWithSalt s . uniqueId

-- instance Show Declare where
--   show = ("dec" ++ ) . show . decUniqueId

data Define = Define
  { defLinkage    :: Maybe Linkage
  , defVisibility :: Maybe Visibility
  , defRetType    :: Type
  , defName       :: Symbol
  , defArgs       :: [Argument]
  , defVarArgs    :: Bool
  , defAttrs      :: [FunAttr]
  , defSection    :: Maybe String
  , defGC         :: Maybe GC
  , defBody       :: [BasicBlock]
  , defUniqueId   :: UniqueId
  , defMetadata   :: FnMdAttachments
  , defComdat     :: Maybe String
  }
  deriving (Generic)

instance Show Define where
  show Define { defName = Symbol x } = "@" ++ x

instance HasUniqueId Define where
  uniqueId = defUniqueId

instance Eq       Define where
  (==) = (==) `on` uniqueId
instance Ord      Define where
  compare = compare `on` uniqueId
instance Hashable Define where
  hashWithSalt s = hashWithSalt s . uniqueId

-- instance Show Define where
--   show = ("def" ++) . show . uniqueId

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
  deriving (Generic, Eq, Ord)

data UnnamedMd = UnnamedMd
  { umIndex    :: Int
  , umValues   :: ValMd
  , umDistinct :: Bool
  }
  deriving (Generic)

instance Eq UnnamedMd where
  (==) = (==) `on` umIndex

instance Ord UnnamedMd where
  compare = compare `on` umIndex

data TypeDecl = TypeDecl
  { typeName  :: Ident
  , typeValue :: Type
  }
  deriving (Generic, Eq, Ord, Show)

-- FIXME: missing parameter attributes
data Argument = Argument
  { argDefine   :: ~Define
  , argName     :: String
  , argType     :: Type
  , argUniqueId :: UniqueId
  , argMd       :: Maybe ValMd
  }
  deriving (Generic)

instance HasUniqueId Argument where
  uniqueId = argUniqueId

instance Eq       Argument where
  (==) = (==) `on` uniqueId
instance Ord      Argument where
  compare = compare `on` uniqueId
instance Hashable Argument where
  hashWithSalt s = hashWithSalt s . uniqueId

instance Show Argument where
  show Argument { argName = x } = x
