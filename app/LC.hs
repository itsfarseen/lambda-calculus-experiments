module LC where

import Data.Char (isNumber)
import Sexpr

data LC v
  = Var v
  | LCInt Int
  | Lam v (LC v)
  | App (LC v) (LC v)
  deriving (Eq)

lamAtom :: SExpr
lamAtom = SAtom "\\."

newtype Id = Id String
  deriving (Eq, Ord)

instance Read Id where
  readsPrec _ s = [(Id s, [])]

instance Show Id where
  show (Id s) = s

instance (Read v) => FromSExpr (LC v) where
  fromSExpr sexpr =
    case sexpr of
      SAtom v -> if all isNumber v then LCInt (read v) else Var (read v)
      SList items -> case items of
        [] -> error "empty parens"
        [x] -> fromSExpr x
        x : xs -> case x of
          x' | x' == lamAtom ->
            case xs of
              [SAtom vstr, body] ->
                Lam (read vstr) (fromSExpr body)
              _ -> error $ "Invalid lambda: " ++ show items
          x' -> case xs of
            [arg] -> App (fromSExpr x') (fromSExpr arg)
            _ -> error $ "Invalid app: " ++ show items

instance (Show v) => ToSExpr (LC v) where
  toSExpr lc = case lc of
    Var v -> SAtom (show v)
    LCInt v -> SAtom (show v)
    Lam v b -> SList [lamAtom, SAtom (show v), toSExpr b]
    App f a -> SList [toSExpr f, toSExpr a]

instance (Read v, Show v) => Read (LC v) where
  readsPrec _ s = [(doRead s, [])]
   where
    doRead s' = fromSExpr (read s')

instance (Read v, Show v) => Show (LC v) where
  show = show . toSExpr

--

type Env v = [(v, LCEval v)]

data LCEval v
  = EvInt Int
  | EvClo (Env v) v (LC v)
  deriving (Show)

evaluate :: (Show v, Eq v) => Env v -> LC v -> Either String (LCEval v)
evaluate env x = case x of
  Var v -> case lookup v env of
    Just v' -> Right v'
    Nothing -> Left $ "Variable not found: " ++ show v
  LCInt v -> pure $ EvInt v
  Lam v b -> pure $ EvClo env v b
  App f a -> do
    f' <- evaluate env f
    case f' of
      EvInt _ -> Left "App first argument must be a function, recvd Int"
      EvClo env' v b -> do
        a' <- evaluate env a
        b' <- evaluate ((v, a') : env') b
        let
         in pure b'
