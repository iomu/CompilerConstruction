{-# LANGUAGE OverloadedStrings #-}

module Emit where

import LLVM.General.Module
import LLVM.General.Context

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP
import qualified LLVM.General.AST.IntegerPredicate as IP

import Data.Maybe
import Data.Word
import Data.Int
import Control.Monad.Except
import Control.Applicative
import qualified Data.Map as Map

import Codegen
import qualified AbsCPP as S

one = cons $ C.Int 32 1
false = cons $ C.Int 1 0
true = cons $ C.Int 1 1

toSig :: [S.Arg] -> [(AST.Type, AST.Name)]
toSig = map (\(S.ADecl t (S.Id x)) -> (fromJust (Map.lookup t types), AST.Name x))

codegenTop :: S.Def -> LLVM ()
codegenTop (S.DFun ty (S.Id name) args body) = do
  t <- lookupInMap ty types
  define t name fnargs bls
  where
    fnargs = toSig args
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM args $ \(S.ADecl ty (S.Id i)) -> do
        t <- lookupInMap ty types
        var <- alloca t
        store var (local t (AST.Name i)) t
        assign i var
      inScope $ cgenStms body


types = Map.fromList [
      (S.Type_double, double),
      (S.Type_int, int),
      (S.Type_bool, bool),
      (S.Type_string, undefined)
  ]

lookupInMap typ values =  case Map.lookup typ values of
  Just x -> return x
  Nothing -> fail "Type not found"

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

addOps = Map.fromList [
      (S.Type_int, iadd),
      (S.Type_double, fadd)
  ]

subOps = Map.fromList [
      (S.Type_int, isub),
      (S.Type_double, fsub)
  ]

cgenEx :: S.Exp -> Codegen AST.Operand
cgenEx (S.ETyped ex t) = case ex of
  S.ETrue           -> return true
  S.EFalse          -> return false
  S.EInt y          -> return $ cons $ C.Int 32 y
  S.EDouble y       -> return $ cons $ C.Float (F.Double y)
  S.EString y       -> undefined
  S.EId (S.Id i)    -> do    
    t' <- lookupInMap t types
    x <- getvar i 
    load x t'
  S.EApp (S.Id fn) args    -> do
    t' <- lookupInMap t types
    largs <- mapM cgenEx args
    call (externf t' (AST.Name fn)) largs t'
  S.EPIncr e@(S.ETyped (S.EId (S.Id var)) _) -> do
    t' <- lookupInMap t types    
    op <- lookupInMap t addOps
    var' <- getvar var
    o <- cgenEx e
    new <- op o one    
    store var' new t'    
    return o 
  S.EAss e1 val -> do
    let (S.ETyped (S.EId (S.Id var)) _) = e1
    t' <- lookupInMap t types
    a <- getvar var 
    cval <- cgenEx val
    store a cval t'
    return cval
cgenEx x = error ":("


cgenBinary fn a b = do
  ca <- cgenEx a
  cb <- cgenEx b
  fn ca cb

cgenStm :: S.Stm -> Codegen ()
cgenStm s = case s of
  S.SExp ex -> void $ cgenEx ex
  S.SDecls typ names -> do
    t <- lookupInMap typ types
    forM_ names $ \(S.Id n) -> do
      var <- alloca t      
      assign n var
  S.SInit typ (S.Id i) ex -> do
    t <- lookupInMap typ types
    o <- cgenEx ex
    var <- alloca t
    un <- assign i var
    void $ store var o t   
  S.SReturn ex -> do
    o <- cgenEx ex
    ret o
    return ()
  S.SReturnVoid -> void retVoid
  S.SWhile ex stm -> do
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
    return ()
  S.SBlock stms -> inScope $ cgenStms stms
  S.SIfElse cond tr fl -> do
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
    return ()


cgenStms = mapM_ cgenStm

expType :: S.Exp -> S.Type
expType (S.ETyped _ t) = t
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
