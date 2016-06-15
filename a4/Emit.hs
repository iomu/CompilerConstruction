{-# LANGUAGE OverloadedStrings #-}

module Emit where

import LLVM.General.Module
import LLVM.General.Context

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP
import qualified LLVM.General.AST.IntegerPredicate as IP

import Data.Word
import Data.Int
import Control.Monad.Except
import Control.Applicative
import qualified Data.Map as Map

import Codegen
import qualified AbsCPP as S

one = const $ C.Int 32 1
false = cons $ C.Int 1 0
true = cons $ C.Int 1 1

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name x))

codegenTop :: S.Def -> LLVM ()
codegenTop (S.DFun ty name args body) = do
  t <- lookupInMap ty type
  define t name fnargs bls
  where
    fnargs = toSig args
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM args $ \S.ADecl ty i -> do
        t <- lookupInMap ty type
        var <- alloca t
        store var (local t (AST.Name i))
        assign i var
      inScope $ cgenStms body


types = Map.fromList [
      (S.Type_double, double),
      (S.Type_int, int),
      (S.Type_bool, bool),
      (S.Type_string, undefined)
  ]

lookupInMap typ values = do
  case Map.lookup typ values of
    Just x -> return x
    Nothing -> fail "Type not found"

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = do
  test <- fcmp FP.ULT a b
  uitofp double test

addOps = Map.fromList [
      (S.Type_int, iadd),
      (S.Type_double, fadd)
  ]

subOps = Map.fromList [
      (S.Type_int, isub),
      (S.Type_double, fsub)
  ]

cgenEx :: S.Exp -> Codegen AST.Operand
cgenEx (ETyped ex t) = case ex of
  ETrue           -> return true
  EFalse          -> return false
  EInt y          -> return $ cons $ Int 32 y
  EDouble y       -> return $ cons $ C.Float (F.Double y)
  EString y       -> undefined
  EId i           -> getvar i >>= load
  EApp fn args    -> do
    largs <- mapM cgenEx args
    call (externf t (AST.Name fn)) largs
  EPIncr e@(ETyped (EId var) _) -> do
    op <- lookupInMap ty addOps
    o <- cgenEx e
    new <- op o one
    var' <- getvar var
    store var' new
    return o
  EIncr e@(ETyped (EId var) _) -> do
    op <- lookupInMap ty addOps
    o <- cgenEx e
    new <- op o one
    var' <- getvar var
    store var' new
    return new   
  EPDecr e@(ETyped (EId var) _) -> do
    op <- lookupInMap ty subOps
    o <- cgenEx e
    new <- op o one
    var' <- getvar var
    store var' new
    return o
  EDecr e@(ETyped (EId var) _) -> do
    op <- lookupInMap ty subOps
    o <- cgenEx e
    new <- op o one
    var' <- getvar var
    store var' new
    return new  
cgen (S.UnaryOp op a) = do
  cgen $ S.Call ("unary" ++ op) [a]
cgen (S.BinaryOp "=" (S.Var var) val) = do
  a <- getvar var
  cval <- cgen val
  store a cval
  return cval
cgen (S.BinaryOp op a b) = do
  case Map.lookup op binops of
    Just f  -> do
      ca <- cgen a
      cb <- cgen b
      f ca cb
    Nothing -> fail "No such operator"
cgen (S.Var x) = getvar x >>= load
cgen (S.Float n) = return $ cons $ C.Float (F.Double n)
cgen (S.Call fn args) = do
  largs <- mapM cgen args
  call (externf (AST.Name fn)) largs

cgenStm :: S.Stm -> Codegen ()
cgenStm s = case s of
  SExp ex -> void $ cgen ex
  SDecls typ names -> do
    t <- lookupType typ
    forM_ names $ \n -> do
      var <- alloca t      
      assign n var
  SInit typ i ex -> do
    t <- lookupType typ
    o <- cgenEx ex
    var <- alloca t
    un <- assign i var
    store un o    
  SReturn ex -> do
    o <- cgen ex
    ret o
  SReturnVoid -> retVoid
  SWhile ex stm -> do
    cond <- addBlock "while.cond"
    body <- addBlock "while.body"
    exit <- addBlock "while.exit"
    br cond
    setBlock cond
    o <- cgenEx ex
    r <- icmp IP.EQ true o
    cbr r body exit
    setBlock body
    cgenStm stm
    br cond
    setBlock exit
  SBlock stms -> inScope $ cgenStms stms
  SIfElse cond tr fl -> do
    ifBlock <- addBlock "if.true"
    elseBlock <- addBlock "if.false"
    exit <- addBlock "if.exit"  
    o <- cgenEx cond
    r <- icmp IP.EQ true o
    cbr r ifBlock elseBlock
    setBlock ifBlock
    cgenStm tr
    br exit
    setBlock elseBlock
    cgenStm fl
    br exit
    setBlock exit


cgenStms = forM_ cgenStm

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> S.Program -> IO AST.Module
codegen mod (S.PDefs fns) = withContext $ \context ->
  liftError $ withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    putStrLn llstr
    return newast
  where
    modn    = mapM codegenTop fns
    newast  = runLLVM mod modn
