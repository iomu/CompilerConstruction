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
    if Map.member var c then
    	throwError $ "Duplicate variable declaration detected: " ++ show var
    else 
    	modify (\s -> s { contexts = Map.insert var t c:cs } )

updateFun :: Id -> FunType -> StateError ()
updateFun name t = do
    sig <- gets signatures
    if Map.member name sig then
    	throwError $ "Duplicate function declaration detected: " ++ show name
    else 
    	modify (\s -> s { signatures = Map.insert name t sig } )

enterScope :: StateError ()
enterScope = modify (\s -> s { contexts = Map.empty:contexts s })

exitScope :: StateError ()
exitScope = modify (\s -> s { contexts = tail (contexts s) })

emptyState :: Env
emptyState = Env Map.empty [] Type_void -- return type gets overwritten whenever we enter a function

inScope :: StateError () -> StateError ()
inScope x = do
    enterScope
    x
    exitScope

setReturnType :: Type -> StateError ()
setReturnType t = modify (\s -> s { returnType = t })
--
typecheck :: Program -> Either String ()
typecheck p = evalErrorState (typecheck' p) emptyState

typecheck' :: Program -> StateError ()
typecheck' (PDefs ds) = do
    mapM_ extendSignatures ds
    forM_ ds checkDef

extendSignatures :: Def -> StateError ()
extendSignatures (DFun t i args _) = updateFun i (map argType args, t)
    where
        argType (ADecl typ _) = typ

checkDef :: Def -> StateError ()
checkDef (DFun t _ args stms) = inScope $
    do
      mapM_ (\(ADecl typ i) -> updateVar i typ) args
      setReturnType t
      checkStms stms
--

inferExp ::  Exp -> StateError Type
inferExp x = case x of
    ETrue           -> return Type_bool
    EFalse          -> return Type_bool
    EInt _          -> return Type_int
    EDouble _       -> return Type_double
    EString _       -> return Type_string
    EId i           -> lookupVar i
    EApp i es       -> do
        (args, typ) <- lookupFun i
        unless (length es == length args) $
            throwError $ "Wrong number of arguments supplied when calling function \""
                ++ show i ++ "\": expected " ++ show (length args) ++ ", found " ++ show (length es)
        zipWithM_ checkExp args es
        return typ
    EPIncr e            -> inferNumUn e
    EPDecr e            -> inferNumUn e
    EIncr e             -> inferNumUn e
    EDecr e             -> inferNumUn e
    ETimes e1 e2        -> inferBin [Type_int, Type_double] e1 e2
    EDiv e1 e2          -> inferBin [Type_int, Type_double] e1 e2
    EPlus e1 e2         -> inferBin [Type_int, Type_double, Type_string] e1 e2
    EMinus e1 e2        -> inferBin [Type_int, Type_double] e1 e2
    ELt e1 e2           -> inferComparison e1 e2
    EGt e1 e2           -> inferComparison e1 e2
    ELtEq e1 e2         -> inferComparison e1 e2
    EGtEq e1 e2         -> inferComparison e1 e2
    EEq e1 e2           -> inferComparison e1 e2
    ENEq e1 e2          -> inferComparison e1 e2
    EAnd e1 e2          -> inferBoolBin e1 e2
    EOr e1 e2           -> inferBoolBin e1 e2
    EAss (EId i) e2    -> do  -- do WE need to check whether the first expression is a variable?
        typ <- lookupVar i
        checkExp typ e2
    EAss e _            -> throwError $ "Left hand side of assignment must be a variable, found " ++ printTree e
    ETyped _ t          -> return t


inferNumUn :: Exp -> StateError Type
inferNumUn = inferUn [Type_int, Type_double]

inferBoolBin ::  Exp -> Exp -> StateError Type
inferBoolBin = inferBin [Type_bool]

inferComparison ::  Exp -> Exp -> StateError Type
inferComparison exp1 exp2 = do
   t1 <- inferExp exp1
   if t1 `elem` [Type_int, Type_double, Type_string, Type_bool] then
      checkExp t1 exp2 >> return Type_bool
    else
      throwError $ "Type of " ++ printTree exp1 ++ " not one of int, bool, string, double"

inferUn :: [Type]  -> Exp -> StateError Type
inferUn types ex = do
    typ <- inferExp ex
    if typ `elem` types then
        return typ
      else
        throwError $ "Type of " ++ printTree ex ++ " not one of " ++ show types

inferBin :: [Type]  -> Exp -> Exp -> StateError Type
inferBin types exp1 exp2 = do
    typ <- inferExp exp1
    if typ `elem` types then
        checkExp typ exp2
    else
        throwError $ "wrong type of expression " ++ printTree exp1

checkExp :: Type -> Exp -> StateError Type
checkExp typ ex = do
    typ2 <- inferExp ex
    if typ2 == typ then
        return typ
    else 
    	throwError $ "Wrong type of \"" ++ printTree ex ++ "\", expected " ++ printTree typ ++ " but found " ++ printTree typ2

checkStm :: Stm -> StateError ()
checkStm x = case x of
    SExp ex -> void (inferExp ex)
    SDecls typ names ->
        mapM_ (\n -> updateVar n typ) names
    SInit typ i ex -> do
    	updateVar i typ
        void $ checkExp typ ex       
    SReturn ex -> do
        t <- gets returnType
        checkExp t ex
        return ()
    SReturnVoid -> do
        t <- gets returnType
        unless (t == Type_void) $
              throwError "Return type of function is not void"
    SWhile ex stm -> do
        checkExp  Type_bool ex
        checkStm stm
    SBlock stms -> inScope $ checkStms stms
    SIfElse ex stm1 stm2 -> do
        checkExp Type_bool ex
        checkStm stm1
        checkStm stm2

checkStms :: [Stm] -> StateError ()
checkStms = mapM_ checkStm
