{-# LANGUAGE RebindableSyntax #-}

module API where

import qualified Prelude as P
import           Copilot.Language hiding (and, or)

cons :: a -> [a] -> [a]
cons = (:)

sing :: a -> [a]
sing = (:[])

ifBool :: Stream Bool -> Stream Bool -> Stream Bool -> Stream Bool
ifBool b x y = if b then x else y

ifWord64 :: Stream Bool -> Stream Word64 -> Stream Word64 -> Stream Word64
ifWord64 b x y = if b then x else y


dropBool :: Int -> Stream Bool -> Stream Bool
dropBool = drop

appBool :: [Bool] -> Stream Bool -> Stream Bool
appBool = (++)

cycleBool :: [Bool] -> Stream Bool
cycleBool xs = let ret = xs ++ ret
               in ret

and' :: Stream Bool -> Stream Bool -> Stream Bool
and' = (&&)

or' :: Stream Bool -> Stream Bool -> Stream Bool
or' = (||)


dropWord64 :: Int -> Stream Word64 -> Stream Word64
dropWord64 = drop

appWord64 :: [Word64] -> Stream Word64 -> Stream Word64
appWord64 = (++)

cycleWord64 :: [Word64] -> Stream Word64
cycleWord64 xs = let ret = xs ++ ret
                 in ret

plusWord64 :: Stream Word64 -> Stream Word64 -> Stream Word64
plusWord64 = (+)

minusWord64 :: Stream Word64 -> Stream Word64 -> Stream Word64
minusWord64 = (-)

timesWord64 :: Stream Word64 -> Stream Word64 -> Stream Word64
timesWord64 = (*)

divWord64 :: Stream Word64 -> Stream Word64 -> Stream Word64
divWord64 = div

signumWord64 :: Stream Word64 -> Stream Word64
signumWord64 = signum

absWord64 :: Stream Word64 -> Stream Word64
absWord64 = abs

eqWord64 :: Stream Word64 -> Stream Word64 -> Stream Bool
eqWord64 = (==)

lteWord64 :: Stream Word64 -> Stream Word64 -> Stream Bool
lteWord64 = (<=)

gtWord64 :: Stream Word64 -> Stream Word64 -> Stream Bool
gtWord64 = (>)


arbiInt :: Int
arbiInt = 5

arbiBool :: Bool
arbiBool = True

arbiListBool :: [Bool]
arbiListBool = [True, False, True, False, True, False, True]

arbiStreamBool :: Stream Bool
arbiStreamBool = cycleBool arbiListBool

arbiWord64 :: Word64
arbiWord64 = 1234

arbiListWord64 :: [Word64]
arbiListWord64 = [1,2,3,4,5,6,7,8,9]

arbiStreamWord64 :: Stream Word64
arbiStreamWord64 = [9,8,7,6,5,4,3,2,1,0] ++ arbiStreamWord64
