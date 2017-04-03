module Main where

import LexStarsepLang
import ParStarsepLang
import AbsStarsepLang
import SkelStarsepLang

import ErrM

main = do
  interact calc
  putStrLn ""

calc s =
  let Ok e = pExpr (myLexer s)
  in show (transExpr e)
