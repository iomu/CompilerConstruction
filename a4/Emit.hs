{-# LANGUAGE OverloadedStrings #-}

module Emit where

import LLVM.General.Module
import LLVM.General.Context

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP
import qualified LLVM.General.AST.IntegerPredicate as IP
import qualified LLVM.General.AST.Type as T

import Data.Maybe
import Data.Word
import Data.Int
import Control.Monad.State
import Control.Monad.Except
import Control.Applicative
import qualified Data.Map as Map

import Codegen
import qualified AbsCPP as S

one = cons $ C.Int 32 1
oneF = cons $ C.Float (F.Double 1.0)

false = cons $ C.Int 1 0
true = cons $ C.Int 1 1

constOneForType t = case t of
  S.Type_int -> one
  S.Type_double -> oneF

toSig :: [S.Arg] -> [(AST.Type, AST.Name)]
toSig = map (\(S.ADecl t (S.Id x)) -> (fromJust (Map.lookup t types), AST.Name x))

{-
  Some notes regarding return statements:
   - at the start of every function we allocate a memory cell of the return type
   - we also create a "return" basic block, in which we return the allocated memory cell
   - whenever we encounter a return statement, we store the expression in the memory cell and then jump to the return block
-}
codegenTop :: S.Def -> LLVM ()
codegenTop (S.DFun ty (S.Id name) args body) = do
  t <- lookupInMap ty types
  define t name fnargs bls
  where
    fnargs = toSig args
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      inScope $ do 
        t <- lookupInMap ty types
        unless (t == T.VoidType) $ do
          r <- alloca t
          initReturnValue r
        rb <- initReturnBlock
        forM_ args $ \(S.ADecl ty (S.Id i)) -> do
          t <- lookupInMap ty types
          var <- alloca t
          store var (local t (AST.Name i)) t
          assign i var        
        cgenStms body
        ht <- hasTerminator        
        unless ht $ void $ br rb        
        setBlock rb
        if t == T.VoidType 
          then retVoid 
          else do
            r <- getReturnValue 
            load r t >>= ret     


types = Map.fromList [
      (S.Type_double, double),
      (S.Type_int, int),
      (S.Type_bool, bool),
      (S.Type_void, T.VoidType)
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

multOps = Map.fromList [
      (S.Type_int, imul),
      (S.Type_double, fmul)
  ]

divOps = Map.fromList [
      (S.Type_int, idiv),
      (S.Type_double, fdiv)
  ]

ltOps = Map.fromList [
      (S.Type_int, icmp IP.SLT),
      (S.Type_bool, bcmp IP.SLT),
      (S.Type_double, fcmp FP.ULT)
  ]

gtOps = Map.fromList [
      (S.Type_int, icmp IP.SGT),
      (S.Type_bool, bcmp IP.SGT),
      (S.Type_double, fcmp FP.UGT)
  ]

lteOps = Map.fromList [
      (S.Type_int, icmp IP.SLE),
      (S.Type_bool, bcmp IP.SLE),
      (S.Type_double, fcmp FP.ULE)
  ]

gteOps = Map.fromList [
      (S.Type_int, icmp IP.SGE),
      (S.Type_bool, bcmp IP.SGE),
      (S.Type_double, fcmp FP.UGE)
  ]

eqOps = Map.fromList [
      (S.Type_int, icmp IP.EQ),
      (S.Type_bool, bcmp IP.EQ),
      (S.Type_double, fcmp FP.UEQ)
  ]

neqOps = Map.fromList [
      (S.Type_int, icmp IP.NE),
      (S.Type_bool, bcmp IP.NE),
      (S.Type_double, fcmp FP.UNE)
  ]

cgenEx :: S.Exp -> Codegen AST.Operand
cgenEx (S.ETyped ex t) = case ex of
  S.ETrue           -> return true
  S.EFalse          -> return false
  S.EInt y          -> return $ cons $ C.Int 32 y
  S.EDouble y       -> return $ cons $ C.Float (F.Double y)  
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
    new <- op o (constOneForType t)   
    store var' new t'    
    return o 
  S.EPDecr e@(S.ETyped (S.EId (S.Id var)) _) -> do
    t' <- lookupInMap t types    
    op <- lookupInMap t subOps
    var' <- getvar var
    o <- cgenEx e
    new <- op o (constOneForType t)    
    store var' new t'    
    return o 
  S.EIncr e@(S.ETyped (S.EId (S.Id var)) _) -> do
    t' <- lookupInMap t types    
    op <- lookupInMap t addOps
    var' <- getvar var
    o <- cgenEx e
    new <- op o (constOneForType t)    
    store var' new t'    
    return new
  S.EDecr e@(S.ETyped (S.EId (S.Id var)) _) -> do
    t' <- lookupInMap t types    
    op <- lookupInMap t subOps
    var' <- getvar var
    o <- cgenEx e
    new <- op o (constOneForType t)    
    store var' new t'    
    return new 
  S.ETimes e1 e2 -> do
    op <- lookupInMap t multOps
    cgenBinary op e1 e2
  S.EDiv e1 e2 -> do
    op <- lookupInMap t divOps
    cgenBinary op e1 e2
  S.EPlus e1 e2 -> do
    op <- lookupInMap t addOps
    cgenBinary op e1 e2
  S.EMinus e1 e2 -> do
    op <- lookupInMap t subOps
    cgenBinary op e1 e2  
  S.ELt e1 e2 -> do
    op <- lookupInMap (expType e1) ltOps
    cgenBinary op e1 e2        
  S.EGt e1 e2 -> do
    op <- lookupInMap (expType e1) gtOps
    cgenBinary op e1 e2  
  S.ELtEq e1 e2 -> do
    op <- lookupInMap (expType e1) lteOps
    cgenBinary op e1 e2  
  S.EGtEq e1 e2 -> do
    op <- lookupInMap (expType e1) gteOps
    cgenBinary op e1 e2  
  S.EEq e1 e2 -> do
    op <- lookupInMap (expType e1) eqOps
    cgenBinary op e1 e2  
  S.ENEq e1 e2 -> do
    op <- lookupInMap (expType e1) neqOps
    cgenBinary op e1 e2  
  S.EAnd e1 e2 -> do
    current <- getBlock
    true <- addBlock "and.true"
    exit <- addBlock "and.exit"    
    a <- cgenEx e1    
    cbr a true exit
    setBlock true
    b <- cgenEx e2    
    br exit
    setBlock exit
    phi bool [(false, current), (b, true)]
  S.EOr e1 e2 -> do
    current <- getBlock
    false <- addBlock "or.false"
    exit <- addBlock "or.exit"    
    a <- cgenEx e1    
    cbr a exit false
    setBlock false
    b <- cgenEx e2    
    br exit
    setBlock exit
    phi bool [(true, current), (b, false)]
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
    assign i var
    void $ store var o t   
  S.SReturn ex -> do
    o <- cgenEx ex
    t <- lookupInMap (expType ex) types
    rv <- getReturnValue
    store rv o t
    rb <- getReturnBlock
    br rb
    return ()
  S.SReturnVoid -> do
    rb <- getReturnBlock
    br rb
    return ()
  S.SWhile ex stm -> do
    cond <- addBlock "while.cond"
    body <- addBlock "while.body"
    exit <- addBlock "while.exit"
    br cond
    setBlock cond
    o <- cgenEx ex    
    cbr o body exit
    setBlock body
    cgenStm stm
    ht <- hasTerminator
    unless ht $ void $ br cond
    setBlock exit
    return ()
  S.SBlock stms -> inScope $ cgenStms stms
  S.SIfElse cond tr fl -> do
    ifBlock <- addBlock "if.true"
    elseBlock <- addBlock "if.false"
    exit <- addBlock "if.exit"  
    o <- cgenEx cond    
    cbr o ifBlock elseBlock
    setBlock ifBlock
    cgenStm tr
    ht <- hasTerminator
    unless ht $ void $ br exit  
    setBlock elseBlock
    cgenStm fl
    ht <- hasTerminator
    unless ht $ void $ br exit
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
