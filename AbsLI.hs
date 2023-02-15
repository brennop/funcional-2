-- Dica Trabalho 2: descomentar abaixo se for usar SYB
{-# LANGUAGE DeriveDataTypeable #-}
-- Haskell data types for the abstract syntax.
-- Generated by the BNF converter.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AbsLI where

-- Dica Trabalho 2: descomentar abaixo se for usar SYB
import Data.Generics
import qualified Data.String
import Prelude (Char, Double, Integer, String)
import qualified Prelude as C (Eq, Ord, Read, Show)

-- Dica Trabalho 2: Ident tambem deve pertencer a type class Data se for usar SYB
newtype Ident = Ident String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString, Data)

newtype Program = Prog [Function]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Function = Fun Ident [Ident] Exp
  deriving (C.Eq, C.Ord, C.Show, C.Read)

-- Dica Trabalho 2: Exp tambem deve pertencer a type class Data se for usar SYB
data Exp
  = EIf Exp Exp Exp
  | EAdd Exp Exp
  | ESub Exp Exp
  | EMul Exp Exp
  | EDiv Exp Exp
  | Call Ident [Exp]
  | EInt Integer
  | EVar Ident
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data)
