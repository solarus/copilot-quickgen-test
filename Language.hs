{-# LANGUAGE TemplateHaskell, TupleSections #-}

module Language where

import           API
import           Copilot.Language hiding (and, or)
import qualified Prelude as P
import           Testing.QuickGen

lang :: Language
lang = $(defineLanguage [| ( cons
                           , sing
                           , ifBool
                           , ifWord64

                           , true
                           , false
                           , not
                           -- , dropBool
                           , appBool
                           , cycleBool
                           , and'
                           , or'

                           -- , dropWord64
                           , appWord64
                           , cycleWord64
                           , plusWord64
                           , minusWord64
                           , timesWord64
                           , divWord64
                           , signumWord64
                           , absWord64
                           , eqWord64
                           , lteWord64
                           , gtWord64

                           , arbiInt
                           , arbiBool
                           , arbiListBool
                           , arbiStreamBool
                           , arbiWord64
                           , arbiListWord64
                           , arbiStreamWord64
                           )
                         |])
