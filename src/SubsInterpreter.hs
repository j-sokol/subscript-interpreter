module SubsInterpreter
       (
         Value(..)
       , runExpr,
       getVar,
       putVar,
       modifyEnv,
       primitiveEq,
       primitiveLt,
       primitiveAnd,
       primitiveMul,
       primitiveSub,
       primitiveMod,
       mkArray,
       getFunction,
       evalExpr,
       evalACFor,
       )
       where

import SubsAst

-- You might need the following imports
import System.IO()
import Text.Printf()
import Control.Monad
import qualified Data.Map as Map
import Data.Map(Map)

-- | A value is either an integer, the special constant undefined,
--   true, false, a string, or an array of values.
-- Expressions are evaluated to values.
data Value = IntVal Int
           | UndefinedVal
           | TrueVal | FalseVal
           | StringVal String
           | ArrayVal [Value]
           deriving (Eq, Show)


type Error = String
type Env = Map Ident Value
type Primitive = [Value] -> Either Error Value
type PEnv = Map FunName Primitive
type Context = (Env, PEnv)

initialContext :: Context
initialContext = (Map.empty, initialPEnv)
  where initialPEnv =
          Map.fromList [ ("===", primitiveEq)
                       , ("<", primitiveLt)
                       , ("+", primitiveAnd)
                       , ("*", primitiveMul)
                       , ("-", primitiveSub)
                       , ("%", primitiveMod)
                       , ("Array", mkArray)
                       ]

newtype SubsM a = SubsM {runSubsM :: Context -> Either Error (a, Env)}

instance Functor SubsM where
  fmap f xs = xs >>= return . f

instance Applicative SubsM where
  pure = return
  (<*>) = ap

instance Monad SubsM where
  return x = SubsM  (\(env, _) -> Right (x, env))
  f >>= m = SubsM   ( \(env, pEnv) -> case runSubsM f (env, pEnv) of
                                      Left e -> Left e
                                      Right (a, env') -> case runSubsM (m a)
                                                        (env', pEnv) of
                                                            Left e -> Left e
                                                            Right v -> Right v )
  fail s = SubsM  (\_ -> Left s)

primitiveEq :: Primitive
primitiveEq [TrueVal, FalseVal] = return FalseVal
primitiveEq [FalseVal, TrueVal] = return FalseVal
primitiveEq [TrueVal, TrueVal]  = return TrueVal
primitiveEq [FalseVal, FalseVal] = return TrueVal
primitiveEq [IntVal num, IntVal num2 ] = if num == num2 then return TrueVal
                                          else return FalseVal

primitiveEq [StringVal s, StringVal s2] = if s == s2 then return TrueVal
                                          else return FalseVal

primitiveEq [ArrayVal a, ArrayVal a2] = if a == a2 then return TrueVal
                                        else return FalseVal

primitiveEq _ = Left "`===` called with wrong number or type of arguments "

primitiveLt :: Primitive
primitiveLt [IntVal num, IntVal num2] = if num < num2 then return TrueVal
                                          else return FalseVal
primitiveLt _ = Left " `<` arguments must be 2 integers"

primitiveAnd :: Primitive
primitiveAnd [IntVal num, IntVal num' ] = return (IntVal (num + num'))
primitiveAnd [StringVal s, StringVal s'] = return (StringVal (s ++ s'))
primitiveAnd [StringVal s, IntVal num] = return (StringVal (s ++ show num))
primitiveAnd [IntVal num, StringVal s] = return (StringVal (show num ++ s ))
primitiveAnd _ = Left "`+` must be called either with 2 ints or strings "

primitiveMul :: Primitive
primitiveMul [IntVal num, IntVal num' ] = return (IntVal  (num * num'))
primitiveMul _ = Left " `*` arguments must be 2 integers"

primitiveSub :: Primitive
primitiveSub [IntVal num, IntVal num' ] = return (IntVal  (num - num'))
primitiveSub _ = Left " `-` arguments must be 2 integers"

primitiveMod :: Primitive
primitiveMod [IntVal num, IntVal num' ] = return (IntVal  (mod num num'))
primitiveMod _ = Left " `%` arguments must be 2 integers"

mkArray :: Primitive
mkArray [IntVal num] | num >= 0 = return $ ArrayVal (replicate num UndefinedVal)
mkArray _ = Left "Array() called with wrong number or type of arguments"

modifyEnv :: (Env -> Env) -> SubsM ()
modifyEnv f = SubsM $ \(env, _) -> Right ((), f env)

putVar :: Ident -> Value -> SubsM ()
putVar name val = modifyEnv (Map.insert name val)

getVar :: Ident -> SubsM Value
getVar name = SubsM (\(env, _) -> case Map.lookup name env of
                                      Just v -> Right (v, env)
                                      Nothing -> Left ("Variable not"++
                                                        " initialised"))

getFunction :: FunName -> SubsM Primitive
getFunction funName = SubsM (\(env, penv) -> case Map.lookup funName penv of
                                          Just p -> Right (p, env)
                                          Nothing -> Left ("unbound function" ++
                                                          "used but not defined"++ funName))

evalExpr :: Expr -> SubsM Value
evalExpr (Number int) = return (IntVal int)
evalExpr (String s) = return (StringVal s)
evalExpr (Array xs) = do
                        vals <- mapM evalExpr xs
                        return $ ArrayVal vals
evalExpr Undefined = return UndefinedVal
evalExpr TrueConst = return TrueVal
evalExpr FalseConst = return FalseVal
evalExpr (Var name)  = getVar name
evalExpr (Compr (ACBody expr)) = evalExpr expr

evalExpr (Compr (ACFor ident expr ac)) = do
                                        iter <- evalExpr expr
                                        -- to save ident val in higher context
                                        tmpIdent <- SubsM (\(env, _) -> 
                                                      case Map.lookup ident env of
                                                        (Just v) -> Right (Just v, env)
                                                        Nothing -> Right (Nothing, env)
                                                      )
                                        -- evaluates 
                                        res <- evalACFor
                                                  ident iter ac

                                        -- put value back into context  or delete        
                                        case tmpIdent of
                                          (Just v) -> putVar ident v
                                          Nothing -> modifyEnv (Map.delete ident)
                                        return res

evalExpr (Compr (ACIf expr comp)) = do
  trueOrFalse <- evalExpr expr
  if trueOrFalse == TrueVal
    then evalExpr (Compr comp)
    else if trueOrFalse == FalseVal
      then return (ArrayVal[])
      else SubsM (const (Left "ACIf must be supplied with a boolean condition"))


--evalExpr (Compr aComp) = evalArrayComp aComp

evalExpr (Call fname params) = do
                                  (ArrayVal values) <- evalExpr (Array params)
                                  function <- getFunction fname
                                  case function values of
                                    (Left e) -> fail e
                                    (Right val) -> return val
evalExpr (Assign name expr) = do
   val <- evalExpr expr
   putVar name val
   return val
evalExpr (Comma e1 e2) = do
                            _ <- evalExpr e1
                            evalExpr e2



evalACFor :: Ident -> Value -> ArrayCompr -> SubsM Value
-- if array to be iterated over is empty
evalACFor _ (ArrayVal []) _ = return $ ArrayVal []
evalACFor ident (StringVal str) compr =
  evalACFor ident (ArrayVal [StringVal [a] | a <- str]) compr
evalACFor ident (ArrayVal (x:xs)) compr = do
      putVar ident x
      val <- evalExpr (Compr compr)
      ArrayVal vals <- evalACFor ident (ArrayVal xs) compr

      -- if there are nested fors, put them together

      if (val == (ArrayVal val')) then do return (ArrayVal ([val] ++ vals))
                                  else do return (ArrayVal (val:vals))


      --vals' <- [val] ++ vals
      --flatten vals'
      --return (ArrayVal (vals'))
      let val' <- (ArrayVal val')
      case val of
        ArrayVal val' -> return $ ArrayVal (val' ++ vals)
        _ -> return $ ArrayVal (val:vals)


evalACFor _ _ _ = fail "ACFor can be iterated only over Array or string"

runExpr :: Expr -> Either Error Value
runExpr expr = case runSubsM (evalExpr expr) initialContext of
  Left e          -> Left e
  Right (val, _) -> Right val
{-
myTest :: IO() -- nezabudnut vymazat
myTest = do
    s <- readFile "intro-ast.txt"
    case runExpr (read s) of
      Left e -> error e
      Right res -> putStrLn $ "Result is: " ++ nice res



nice :: Value -> String -- nezabudnut vymazat
nice (IntVal v) = show v
nice TrueVal = "true"
nice FalseVal = "false"
nice (StringVal s) = show s
nice UndefinedVal = "undefined"
nice (ArrayVal vs) = "["++ intercalate ", " (map nice vs) ++"]" 
-}
