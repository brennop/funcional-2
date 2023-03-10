{- Programacao Funcional - Trabalho 2 -
   Prazo de entrega: 19/02/2023 por Tarefa no Aprender 3
   O trabalho deve ser feito individualmente

  ** ENUNCIADO DO TRABALHO:
 Evolua o interpretador abaixo para prover avaliacao preguicosa (lazy evaluation).
 Ao fim do codigo abaixo, ha alguns casos de testes.
 Sugere-se observar as dicas neste arquivo e no AbsLI.hs.

 Criterios de avaliacao:
   1) testCaseSuiteResults deve ser computavel e ser True
   2) para nota maior ou igual a 9, deve-se usar SYB.

-}

module Interpreter where

import AbsLI
import Data.Generics
import Data.Maybe
import Memo
import Prelude

type Context k v = [(k, v)]

type RContext = (VContext, FContext)

type LContext = (EContext, FContext)

type VContext = Context Ident Integer

type FContext = Context Ident Function

type EContext = Context Ident Exp'

evalP :: Program -> Integer
evalP (Prog fs) = eval' (evalP' (Prog fs))

evalP' :: Program -> Exp'
evalP' (Prog fs) = evalLazy ([], updatecF [] fs) (Call (Ident "main") [])

eval :: RContext -> Exp -> Integer
eval context x = case x of
  EAdd exp0 exp -> eval context exp0 + eval context exp
  ESub exp0 exp -> eval context exp0 - eval context exp
  EMul exp0 exp -> eval context exp0 * eval context exp
  EDiv exp0 exp -> eval context exp0 `div` eval context exp
  EInt n -> n
  -- dica: considere alteracao na alternativa abaixo
  EVar id -> fromJust (lookupMemo id (fst context))
  EIf e1 e2 e3 ->
    if eval context e1 /= 0
      then eval context e2
      else eval context e3
  -- dica: considere alteracao na alternativa abaixo
  Call id lexp -> eval (paramBindings, contextFunctions) exp
    where
      Fun _ decls exp = fromJust (lookupMemo id (snd context))
      paramBindings = zip decls (map (eval context) lexp)
      contextFunctions = snd context

eval' :: Exp' -> Integer
eval' x = case x of
  EAdd' exp0 exp -> eval' exp0 + eval' exp
  ESub' exp0 exp -> eval' exp0 - eval' exp
  EMul' exp0 exp -> eval' exp0 * eval' exp
  EDiv' exp0 exp -> eval' exp0 `div` eval' exp
  EInt' n -> n
  EIf' e1 e2 e3 ->
    if eval' e1 /= 0
      then eval' e2
      else eval' e3

evalLazy :: LContext -> Exp -> Exp'
evalLazy context x = case x of
  EAdd exp0 exp -> EAdd' (evalLazy context exp0) (evalLazy context exp)
  ESub exp0 exp -> ESub' (evalLazy context exp0) (evalLazy context exp)
  EMul exp0 exp -> EMul' (evalLazy context exp0) (evalLazy context exp)
  EDiv exp0 exp -> EDiv' (evalLazy context exp0) (evalLazy context exp)
  EInt n -> EInt' n
  EVar id -> fromJust (lookupMemo id (fst context))
  EIf e1 e2 e3 -> EIf' (evalLazy context e1) (evalLazy context e2) (evalLazy context e3)
  Call id lexp -> evalLazy (paramBindings, contextFunctions) exp
    where
      Fun _ decls exp = fromJust (lookupMemo id (snd context))
      paramBindings = zip decls (map (evalLazy context) lexp)
      contextFunctions = snd context

updatecF :: FContext -> [Function] -> FContext
updatecF ctx [] = ctx
updatecF ctx (f@(Fun id _ _) : fs) = updatecF (updateMemo ctx id f) fs

{-
  main () {
    fat (5)
  }

  fat (n) {
    if (n)
       then n * fat (n - 1)
       else 1
  }
-}

fat =
  Prog
    [ Fun (Ident "main") [] (Call (Ident "fat") [EInt 5]),
      Fun
        (Ident "fat")
        [Ident "n"]
        ( EIf
            (EVar (Ident "n"))
            ( EMul
                (EVar (Ident "n"))
                (Call (Ident "fat") [ESub (EVar (Ident "n")) (EInt 1)])
            )
            (EInt 1)
        )
    ]

testCaseFat = evalP fat == 120

{-
 main () {
   fib (8)
}
 fib (n) {
   if (n) then
      if (n - 1)
        then fib (n - 1) + fib (n - 2)
        else 1
    else 1
}
-}

fibo =
  Prog
    [ Fun (Ident "main") [] (Call (Ident "fib") [EInt 8]),
      Fun
        (Ident "fib")
        [Ident "n"]
        ( EIf
            (EVar (Ident "n"))
            ( EIf
                (ESub (EVar (Ident "n")) (EInt 1))
                ( EAdd
                    (Call (Ident "fib") [ESub (EVar (Ident "n")) (EInt 1)])
                    (Call (Ident "fib") [ESub (EVar (Ident "n")) (EInt 2)])
                )
                (EInt 1)
            )
            (EInt 1)
        )
    ]

testCaseFibo = evalP fibo == 34

-- testCaseSuiteResults deve ser true
testCaseSuiteResults = testCaseFat && testCaseFibo
