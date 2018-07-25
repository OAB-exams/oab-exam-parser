module Main
  (main)
where

import Exam.XML
import Exam.Parse
import Exam.IO

import System.Environment

main :: IO ()
main = do
  (c:as) <- getArgs
  case c of
    "xml" -> mapM_ readAndPrintExam as
    _ -> return ()
