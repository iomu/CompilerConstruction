{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import Data.Maybe
import Data.Word
import Data.String
import Data.List
import Data.Function
import qualified Data.Map as Map

import Control.Monad.State
import Control.Applicative
import LLVM.General.AST

import qualified LLVM.General.AST.Type as T
import LLVM.General.AST.Global
import qualified LLVM.General.AST as AST

import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.FloatingPointPredicate as FP
import qualified LLVM.General.AST.IntegerPredicate as IP

-------------------------------------------------------------------------------
-- Module Level
-------------------------------------------------------------------------------

newtype LLVM a = LLVM { unLLVM :: State AST.Module a }
  deriving (Functor, Applicative, Monad, MonadState AST.Module )

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM = flip (execState . unLLVM)

emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

define ::  Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = body
  }

---------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

double :: Type
double = T.double
-- Int
int :: Type
int = T.i32

-- Boolean
bool :: Type
bool = T.i1


-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

type Names = Map.Map String Int

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm,  Map.insert nm 1 ns)
    Just ix -> (nm ++ show ix, Map.insert nm (ix+1) ns)

instance IsString Name where
  fromString = Name . fromString

-------------------------------------------------------------------------------
-- Codegen State
-------------------------------------------------------------------------------

type SymbolTable = [(String, Operand)]

data CodegenState
  = CodegenState {
    currentBlock :: Name                     -- Name of the active block to append to
  , blocks       :: Map.Map Name BlockState  -- Blocks for function
  , symtabs      :: [SymbolTable]              -- Function scope symbol table
  , blockCount   :: Int                      -- Count of basic blocks
  , count        :: Word                     -- Count of unnamed instructions
  , names        :: Names 
  , returnValue   :: Maybe Operand                   -- Name Supply 
  , returnBlock  :: Maybe Name        
  } deriving Show

data BlockState
  = BlockState {
    idx   :: Int                            -- Block index
  , stack :: [Named Instruction]            -- Stack of instructions
  , term  :: Maybe (Named Terminator)       -- Block terminator
  } deriving Show

enterScope :: Codegen ()
enterScope = modify (\s -> s { symtabs = []:symtabs s })

exitScope :: Codegen ()
exitScope = modify (\s -> s { symtabs = tail (symtabs s) })

inScope :: Codegen a -> Codegen a
inScope x = do
    enterScope
    r <- x
    exitScope
    return r

initReturnValue :: Operand -> Codegen ()
initReturnValue op = modify $ \s -> s { returnValue = Just op }
  
initReturnBlock :: Codegen Name
initReturnBlock = do
  rb <- addBlock "return"
  modify $ \s -> s { returnBlock = Just rb }
  return rb

getReturnValue :: Codegen Operand
getReturnValue = do
  r <- gets returnValue
  case r of
    Just x -> return x
    Nothing -> fail "No return value initialized"

getReturnBlock :: Codegen Name
getReturnBlock = do
  r <- gets returnBlock
  case r of
    Just x -> return x
    Nothing -> fail "No return block initialized"


-------------------------------------------------------------------------------
-- Codegen Operations
-------------------------------------------------------------------------------

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)
  where
    isEmptyBlock (_, BlockState _ s t) = null s && isNothing t

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, BlockState _ s t) = BasicBlock l s (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ show l

entryBlockName :: String
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name entryBlockName) Map.empty [] 1 0 Map.empty Nothing Nothing

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = 1 + i }
  return $ i + 1

instr :: Type -> Instruction -> Codegen Operand
instr t ins = do
  n <- fresh
  let ref = UnName n
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = i ++ [ref := ins] } )
  return $ local t ref

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })
  return trm

hasTerminator :: Codegen Bool
hasTerminator = do
  b <- current  
  return $ isJust $ term b

-------------------------------------------------------------------------------
-- Block Stack
-------------------------------------------------------------------------------

entry :: Codegen Name
entry = gets currentBlock

addBlock :: String -> Codegen Name
addBlock bname = do
  bls <- gets blocks
  ix <- gets blockCount
  nms <- gets names
  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms
  modify $ \s -> s { blocks = Map.insert (Name qname) new bls
                   , blockCount = ix + 1
                   , names = supply
                   }
  return (Name qname)

setBlock :: Name -> Codegen Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show c

-------------------------------------------------------------------------------
-- Symbol Table
-------------------------------------------------------------------------------

assign :: String -> Operand -> Codegen ()
assign var x = do 
  (c:sts) <- gets symtabs  
  modify $ \s -> s { symtabs = ((var, x):c):sts }
  

getvar :: String -> Codegen Operand
getvar var = do
  syms <- gets symtabs
  let firstMatch =  foldl (<|>) Nothing (map (lookup var) syms)
  
  case firstMatch of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var
  
-------------------------------------------------------------------------------

-- References
local ::  Type -> Name -> Operand
local = LocalReference 

global :: Type -> Name -> C.Constant
global = C.GlobalReference 

externf :: Type -> Name -> Operand
externf t = ConstantOperand . C.GlobalReference t

-- Arithmetic and Constants

-- Int
icmp :: IP.IntegerPredicate -> Operand -> Operand -> Codegen Operand
icmp cond a b = instr bool $ ICmp cond a b []

iadd :: Operand -> Operand -> Codegen Operand
iadd a b = instr int $ Add False False a b []

isub :: Operand -> Operand -> Codegen Operand
isub a b = instr int $ Sub False False a b []

imul :: Operand -> Operand -> Codegen Operand
imul a b = instr int $ Mul False False a b []

idiv :: Operand -> Operand -> Codegen Operand
idiv a b = instr int $ SDiv False a b []

-- Double
fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instr double $ FAdd NoFastMathFlags a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr double $ FSub NoFastMathFlags a b []

fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instr double $ FMul NoFastMathFlags a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr double $ FDiv NoFastMathFlags a b []

fcmp :: FP.FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
fcmp cond a b = instr bool $ FCmp cond a b []

-- Bool
bcmp :: IP.IntegerPredicate -> Operand -> Operand -> Codegen Operand
bcmp cond a b = instr bool $ ICmp cond a b []

cons :: C.Constant -> Operand
cons = ConstantOperand

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

-- Effects
call :: Operand -> [Operand] -> Type -> Codegen Operand
call fn args ty = instr ty $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []

alloca :: Type -> Codegen Operand
alloca ty = instr ty $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Type -> Codegen Operand
store ptr val t = instr t $ Store False ptr val Nothing 0 []

load :: Operand -> Type -> Codegen Operand
load ptr t = instr t $ Load False ptr Nothing 0 []

-- Control Flow
br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

phi :: Type -> [(Operand, Name)] -> Codegen Operand
phi ty incoming = instr ty $ Phi ty incoming []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

retVoid :: Codegen (Named Terminator)
retVoid = terminator $ Do $ Ret Nothing []
