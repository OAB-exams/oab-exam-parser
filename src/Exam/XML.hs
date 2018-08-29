{-# LANGUAGE OverloadedStrings #-}
module Exam.XML
  where

import Exam.Type

import Text.XML.Generator

import Data.Text

xattrs' :: [(Name, TextContent)] -> Xml Attr
xattrs' = xattrs . fmap (uncurry xattr)

examToXML :: XmlOutput t => Exam -> t
examToXML e = xrender
  $ doc defaultDocInfo root
 where
   root = xelem "OAB-exam"
     $ xattrs' ["year" <#> year e,
                "edition" <#> edition e]
     <#> qs
   qs = xelems $ fmap question $ questions e

question :: Question -> Xml Elem
question q = xelem "question"
  $ attrs
  <#> xelems ((xelem "statement" $ xtext (instr q)) : [is])
  where
    attrs = xattrs' $ (("number" <#> number q)
                       : ("valid" <#> if valid q
                                      then "true"
                                      else "false")
                       : areaAttr)
    areaAttr = let ar = area q in if Prelude.null ar
                                  then []
                                  else ["area" <#> intercalate " " (area q)]
    is = xelems $ fmap item (items q)
    item i = xelem "item"
      $ xattrs' ["letter" <#> letter i,
                 "correct" <#> if correct i then "true" else "false"]
      <#> xtext (text i)
