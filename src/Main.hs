module Main where

import LexStarsepLang
import ParStarsepLang
import AbsStarsepLang
import Interpreter
import Environment
import ErrM

main = do
  interact interpreter
  putStrLn ""

interpreter s =
  let Ok e = pProgram (myLexer s)
  in show (transProgram e)
