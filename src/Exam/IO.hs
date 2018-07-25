{-# LANGUAGE OverloadedStrings #-}
module Exam.IO
where

import Exam.Type
import Exam.Parse
import Exam.XML

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text.IO as TI

readAndPrintExam :: FilePath -> IO ()
readAndPrintExam fp = do
  readExam fp >>= printExam
  return ()

readExam :: FilePath -> IO Exam
readExam fp = do
  et <- TI.readFile fp
  case parseExam fp et of
    Left err -> putStrLn err *> return (Exam "" "" [])
    Right e -> return e

printExam :: Exam -> IO ()
printExam = BSL.putStrLn . examToXML
