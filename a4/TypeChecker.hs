-- Authors Lublow, Müller, Zahn

module TypeChecker where

import           AbsCPP
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans.State
import qualified Data.Map.Lazy             as Map
import           Prelude                   hiding (lookup)
import           PrintCPP

type StateError a = StateT Env (Except String) a

evalErrorState :: StateError a -> Env -> Either String a
evalErrorState m = runExcept . evalStateT m

data Env = Env { signatures :: Sig,
                 contexts   :: [Context],
                 returnType :: Type }

type FunType = ([Type], Type)
type Sig = Map.Map Id FunType
type Context = Map.Map Id Type

lookupVar :: Id -> StateError Type
lookupVar name = do
    cs <- gets contexts
    let firstMatch =  foldl (<|>) Nothing (map (Map.lookup name) cs)
    maybe err return firstMatch
    where
        err = throwError $ "Variable " ++ show name ++ " not found."

lookupFun :: Id -> StateError FunType
lookupFun name = do
    ss <- gets signatures
    maybe err return (Map.lookup name ss)
  where
    err = throwError $ "Function " ++ show name ++ " not found."

updateVar :: Id -> Type -> StateError ()
updateVar var t = do
    (c:cs) <- gets contexts
    if Map.member var c 
     then throwError ("Duplicate variable declaration detected: " ++ show var)
     else modify (\s -> s { contexts = Map.insert var t c:cs } )

updateFun :: Id -> FunType -> StateError ()
updateFun name t = do
    sig <- gets signatures
    if Map.member name sig 
     then throwError ( "Duplicate function declaration detected: " ++ show name)
     else modify (\s -> s { signatures = Map.insert name t sig } )

enterScope :: StateError ()
enterScope = modify (\s -> s { contexts = Map.empty:contexts s })

exitScope :: StateError ()
exitScope = modify (\s -> s { contexts = tail (contexts s) })

emptyState :: Env
emptyState = Env Map.empty [] Type_void -- return type gets overwritten whenever we enter a function

inScope :: StateError a -> StateError a
inScope x = do
    enterScope
    r <- x
    exitScope
    return r

setReturnType :: Type -> StateError ()
setReturnType t = modify (\s -> s { returnType = t })
--
typecheck :: Program -> Either String Program
typecheck p = evalErrorState (typecheck' p) emptyState

typecheck' :: Program -> StateError Program
typecheck' (PDefs ds) = do
    mapM_ extendSignatures ds
    defs <- forM ds checkDef
    return $ PDefs defs

extendSignatures :: Def -> StateError ()
extendSignatures (DFun t i args _) = updateFun i (map argType args, t)
    where
        argType (ADecl typ _) = typ

checkDef :: Def -> StateError Def
checkDef (DFun t n args stms) = inScope $
    do
      mapM_ (\(ADecl typ i) -> updateVar i typ) args
      setReturnType t
      ss <- checkStms stms
      return $ DFun t n args ss
--

inferExp ::  Exp -> StateError Exp
inferExp x = case x of
    ETrue           -> return $ ETyped ETrue Type_bool
    EFalse          -> return $ ETyped EFalse Type_bool
    EInt y          -> return $ ETyped (EInt y) Type_int
    EDouble y       -> return $ ETyped (EDouble y) Type_double
    EString y       -> return $ ETyped (EString y) Type_string
    EId i           -> do
        t <- lookupVar i
        return $ ETyped (EId i) t
    EApp i es       -> do
        (args, typ) <- lookupFun i
        unless (length es == length args) $
            throwError $ "Wrong number of arguments supplied when calling function \""
                ++ show i ++ "\": expected " ++ show (length args) ++ ", found " ++ show (length es)
        es' <- zipWithM checkExp args es
        return $ ETyped (EApp i es') typ
    EPIncr e            -> inferNumUn e
    EPDecr e            -> inferNumUn e
    EIncr e             -> inferNumUn e
    EDecr e             -> inferNumUn e
    ETimes e1 e2        -> do
        (e1', e2', t) <- inferBin [Type_int, Type_double] e1 e2
        return $ ETyped (ETimes e1' e2') t
    EDiv e1 e2          -> do
        (e1', e2', t) <- inferBin [Type_int, Type_double] e1 e2
        return $ ETyped (EDiv e1' e2') t
    EPlus e1 e2         -> do
        (e1', e2', t) <- inferBin [Type_int, Type_double, Type_string] e1 e2
        return $ ETyped (EPlus e1' e2') t
    EMinus e1 e2        -> do
        (e1', e2', t) <- inferBin [Type_int, Type_double] e1 e2
        return $ ETyped (EMinus e1' e2') t
    ELt e1 e2           -> do
        (e1', e2', t) <- inferComparison e1 e2
        return $ ETyped (ELt e1' e2') t
    EGt e1 e2           -> do
        (e1', e2', t) <- inferComparison e1 e2
        return $ ETyped (EGt e1' e2') t
    ELtEq e1 e2         -> do
        (e1', e2', t) <- inferComparison e1 e2
        return $ ETyped (ELtEq e1' e2') t
    EGtEq e1 e2         -> do
        (e1', e2', t) <- inferComparison e1 e2
        return $ ETyped (EGtEq e1' e2') t
    EEq e1 e2           -> do
        (e1', e2', t) <- inferComparison e1 e2
        return $ ETyped (EEq e1' e2') t
    ENEq e1 e2          -> do
        (e1', e2', t) <- inferComparison e1 e2
        return $ ETyped (ENEq e1' e2') t
    EAnd e1 e2          -> do
        (e1', e2', t) <- inferBoolBin e1 e2
        return $ ETyped (EAnd e1' e2') t
    EOr e1 e2           -> do
        (e1', e2', t) <- inferBoolBin e1 e2
        return $ ETyped (EOr e1' e2') t
    EAss e@(EId i) e2    -> do  -- do WE need to check whether the first expression is a variable?
        typ <- lookupVar i
        e2' <- checkExp typ e2
        return $ ETyped (EAss e e2') typ
    EAss e _            -> throwError $ "Left hand side of assignment must be a variable, found " ++ printTree e
    ETyped ex t          -> return $ ETyped ex t


inferNumUn :: Exp -> StateError Exp
inferNumUn = inferUn [Type_int, Type_double]

inferBoolBin ::  Exp -> Exp -> StateError (Exp, Exp, Type) 
inferBoolBin = inferBin [Type_bool]

inferComparison ::  Exp -> Exp -> StateError (Exp, Exp, Type)
inferComparison exp1 exp2 = do
   e@(ETyped exp1' t1) <- inferExp exp1
   if t1 `elem` [Type_int, Type_double, Type_string, Type_bool] then
      do
      exp2' <- checkExp t1 exp2 
      return (e, exp2', Type_bool)
    else
      throwError $ "Type of " ++ printTree exp1 ++ " not one of int, bool, string, double"

inferUn :: [Type]  -> Exp -> StateError Exp
inferUn types ex = do
    ETyped ex' typ <- inferExp ex
    if typ `elem` types then
        return $ ETyped ex' typ
      else
        throwError $ "Type of " ++ printTree ex ++ " not one of " ++ show types

inferBin :: [Type]  -> Exp -> Exp -> StateError (Exp, Exp, Type)
inferBin types exp1 exp2 = do
    e@(ETyped exp1' typ) <- inferExp exp1
    if typ `elem` types then
        do
            exp2' <- checkExp typ exp2
            return (e, exp2', typ)
     else
        throwError $ "wrong type of expression " ++ printTree exp1

checkExp :: Type -> Exp -> StateError Exp
checkExp typ ex = do
    l@(ETyped ex' typ2) <- inferExp ex
    if typ2 == typ then
        return l
     else 
        throwError $ "Wrong type of \"" ++ printTree ex ++ "\", expected " ++ printTree typ ++ " but found " ++ printTree typ2

checkStm :: Stm -> StateError Stm
checkStm x = case x of
    SExp ex -> do
        ETyped ex' t <- inferExp ex
        return $ SExp ex'
    ds@(SDecls typ names) -> do
        mapM_ (\n -> updateVar n typ) names
        return ds
    SInit typ i ex -> do
        updateVar i typ
        ex' <- checkExp typ ex  
        return $ SInit typ i ex'     
    SReturn ex -> do
        t <- gets returnType
        ex' <- checkExp t ex
        return $ SReturn ex'
    SReturnVoid -> do
        t <- gets returnType
        unless (t == Type_void) $
              throwError "Return type of function is not void"
        return SReturnVoid
    SWhile ex stm -> do
        ex' <- checkExp Type_bool ex
        stm' <- checkStm stm
        return $ SWhile ex' stm'
    SBlock stms -> fmap SBlock $ inScope $ checkStms stms
    SIfElse ex stm1 stm2 -> do
        ex' <- checkExp Type_bool ex
        stm1' <- checkStm stm1
        stm2' <- checkStm stm2
        return $ SIfElse ex' stm1' stm2'

checkStms :: [Stm] -> StateError [Stm]
checkStms = mapM checkStm
