module Main where

import LexStarsepLang
import ParStarsepLang
import AbsStarsepLang
import Interpreter

import ErrM

main = do
  interact calc
  putStrLn ""

calc s =
  let Ok e = pProgram (myLexer s)
  in show (transProgram e)
