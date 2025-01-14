module Sexpr (
  SExpr (..),
  FromSExpr (..),
  ToSExpr (..),
) where

import Data.Char (isSpace)

data SExpr
  = SAtom String
  | SList [SExpr]
  deriving (Eq)

instance Read SExpr where
  readsPrec _ str = case tokenize str of
    [] -> []
    ws -> case parseSExpr ws of
      (Just expr, []) -> [(expr, [])]
      _ -> []

tokenize :: String -> [String]
tokenize [] = []
tokenize ('(' : xs) = "(" : tokenize xs
tokenize (')' : xs) = ")" : tokenize xs
tokenize (x : xs)
  | isSpace x = tokenize xs
  | otherwise =
      let (token, rest) =
            span
              (\c -> not (isSpace c) && c /= '(' && c /= ')')
              (x : xs)
       in token : tokenize rest

parseSExpr :: [String] -> (Maybe SExpr, [String])
parseSExpr [] = (Nothing, [])
parseSExpr ("(" : rest) = parseList rest []
parseSExpr (w : rest) = (Just $ SAtom w, rest)

parseList :: [String] -> [SExpr] -> (Maybe SExpr, [String])
parseList (")" : rest) acc = (Just $ SList (reverse acc), rest)
parseList [] _ = (Nothing, [])
parseList ws acc = case parseSExpr ws of
  (Just expr, rest) -> parseList rest (expr : acc)
  (Nothing, rest) -> (Nothing, rest)

instance Show SExpr where
  show sexpr =
    case sexpr of
      SAtom s -> s
      SList items -> "(" ++ unwords (map show items) ++ ")"

class FromSExpr a where
  fromSExpr :: SExpr -> a

  readSExpr :: String -> a
  readSExpr s = fromSExpr (read s)

class ToSExpr a where
  toSExpr :: a -> SExpr

  showSExpr :: a -> String
  showSExpr a = show (toSExpr a)
