module Lib
  ( runProgram, RunResult (..), StepResult (..)
  , EvalOptions (..), defaultEvalOptions, initialEnv
  ) where

import Evaluator (EvalOptions (..), defaultEvalOptions, initialEnv)
import Program (RunResult (..), StepResult (..), runProgram)
