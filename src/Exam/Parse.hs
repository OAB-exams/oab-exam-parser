{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      :  Exam.Parse
-- Copyright   :  © 2018 bruno cuconato
-- License     :  LPGL-3
--
-- Maintainer  :  bruno cuconato <bcclaro+hackage@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- parse OAB exams

module Exam.Parse
where

---
-- imports
import Exam.Type

import Control.Monad (void)
import qualified Data.Text as T
import Data.Char
import Data.Either
import Data.Maybe
import Data.Void (Void)
import System.FilePath

import Text.Megaparsec
       (ParseError, Parsec, (<?>), between, choice, sepBy1, eof,
        lookAhead, many, optional, parse, parseErrorPretty,
        some, takeWhileP, someTill, skipManyTill, try, withRecovery)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


-- * Parsers
-- | Parser type synonym
type Parser = Parsec Void T.Text

-- | Parser raw output
type RawData t e = [Either (ParseError t e) Question]

---
-- exam parsers
examP :: Parser (RawData Char Void)
examP = between ws eof $ some e
  where
    e = withRecovery recover (Right <$> questionP)
    recover err = Left err <$
      skipManyTill anyChar (try $ lookAhead (symbol "\n" <* symbol "\n" <* symbol' "Questão" <?> "Consume question with error."))

questionP :: Parser Question
questionP = do
  symbol' "Questão"
  n <- numberP
  v <- validP
  a <- areaP
  inst <- instructionP
  is <- optionsP
  return $ Question v n a inst is

validP :: Parser Bool
validP = fmap isNothing $ optional $ symbol' "NULL"

numberP :: Parser T.Text
numberP = do
  n <- lexeme (some digitChar) <?> "question number"
  return $ T.pack n

areaP :: Parser Area
areaP = symbol' "AREA" *> (many $ choice $ fmap symbol' [
  "ADMINISTRATIVE",
  "BUSINESS",
  "CHILDREN",
  "CIVIL",
  "CIVIL-PROCEDURE",
  "CONSTITUTIONAL",
  "CONSUMER",
  "CRIMINAL",
  "CRIMINAL-PROCEDURE",
  "ENVIRONMENTAL",
  "ETHICS",
  "HUMAN-RIGHTS",
  "INTERNATIONAL",
  "LABOUR",
  "LABOUR-PROCEDURE",
  "PHILOSOPHY",
  "TAXES"
  ])

instructionP :: Parser Instr
instructionP = fmap mconcat $ someTill paragraph (symbol' "OPTIONS")

optionsP :: Parser [Item]
optionsP = collect $ fmap itemP ["A", "B", "C", "D"]
  where
    itemP c = do
      l <- symbol c
      b <- optional $ symbol ":CORRECT"
      _ <- symbol ")"
      i <- paragraph
      ws
      return $ Item l (isJust b) i

---
-- util parsers
paragraph :: Parser T.Text
paragraph = fmap T.pack . lexeme $ par
  where
    par :: Parser String
    par = do
      c <- anyChar
      if c == '\n'
        then do
        cs <- sawNewline
        return $ c:cs
        else do
        cs <- par
        return $ c:cs
    sawNewline :: Parser String
    sawNewline = do
        ss <- many spaceChar
        if '\n' `elem` ss
          then
          return ""
          else do
          cs <- par
          return cs

collect :: [Parser a] -> Parser [a]
collect [] = return []
collect (p:ps) = do
  a <- p
  as <- collect ps
  return $ a:as

---
-- lexing
symbol :: T.Text -> Parser T.Text
symbol = L.symbol ws

symbol' :: T.Text -> Parser T.Text
symbol' = L.symbol' ws

lexeme :: Parser a -> Parser a
lexeme = L.lexeme ws

ws :: Parser ()
ws = void $ takeWhileP (Just "space") isSpace

---
-- * parse
parseExam :: FilePath -> T.Text -> Either String Exam
parseExam fp' s =
  let fp = takeFileName fp'
      year = T.pack $ take 4 fp
      edition = T.pack $ take 2 $ drop 5 fp
  in
    case parse examP fp' s of
      Left err -> Left $ parseErrorPretty err
      Right d ->
        let (ls, rs) = partitionEithers d
        in if null ls
           then Right $ Exam year edition rs
           else Left $ concatMap parseErrorPretty ls
