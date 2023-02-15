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

type LContext = (EContext, FContext)

type FContext = Context Ident Function

type EContext = Context Ident Exp

evalP :: Program -> Integer
evalP (Prog fs) = eval (evalL ([], updatecF [] fs) (Call (Ident "main") []))

eval :: Exp -> Integer
eval x = case x of
  EAdd exp0 exp -> eval exp0 + eval exp
  ESub exp0 exp -> eval exp0 - eval exp
  EMul exp0 exp -> eval exp0 * eval exp
  EDiv exp0 exp -> eval exp0 `div` eval exp
  EInt n -> n
  EIf e1 e2 e3 ->
    if eval e1 /= 0
      then eval e2
      else eval e3
  -- evalL deve ter substituido as variaveis e chamadas de funcao
  -- por suas respectivas expressoes
  EVar id -> error "variavel nao definida"
  Call id exps -> error "funcao nao definida"

evalL :: LContext -> Exp -> Exp
evalL context = everywhere (mkT expr)
  where
    expr (EVar id) = fromJust (lookupMemo id (fst context))
    expr (Call id exps) = evalL (binds, ctx) exp
      where
        (Fun _ args exp) = fromJust (lookupMemo id (snd context))
        binds = zip args exps
        ctx = snd context
    expr e = e

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
