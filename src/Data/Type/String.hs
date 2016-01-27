{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}

module Data.Type.String
  ( TChar
  , TString
  , Chr
  , Str
  , qChr
  , qStr
  ) where

import Type.Class.Higher
import Type.Class.Witness
import Type.Family.Bool
import Type.Family.List
import Type.Family.Nat
import Data.Type.Boolean
import Data.Type.Product
import Data.Type.Quote
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import qualified Data.Char as Char

data Chr = Char Bool Bool Bool Bool Bool Bool Bool Bool

type instance ('Char a0 a1 a2 a3 a4 a5 a6 a7)
           == ('Char b0 b1 b2 b3 b4 b5 b6 b7)
  =  a0 == b0 && a1 == b1 && a2 == b2 && a3 == b3
  && a4 == b4 && a5 == b5 && a6 == b6 && a7 == b7

data TChar :: Chr -> * where
  TChar :: !(Prod Boolean '[b0,b1,b2,b3,b4,b5,b6,b7])
        -> TChar ('Char b0 b1 b2 b3 b4 b5 b6 b7)

toChar :: TChar c -> Char
toChar (TChar ds) = Char.chr $ fromBin $ toList toBool ds

instance Show (TChar c) where
  showsPrec d c = showParen (d > 10)
    $ showString "TChar "
    . shows (toChar c)

instance Show1 TChar

instance TestEquality TChar where
  testEquality (TChar as) (TChar bs) = as =?= bs //? qed

instance BoolEquality TChar where
  boolEquality (TChar as) (TChar bs) = as .== bs

-- Chr QQ {{{

qChr :: QuasiQuoter
qChr = QuasiQuoter
  { quoteExp  = quoteTCharE qq
  , quotePat  = quoteTCharP qq
  , quoteType = quoteCharT qq
  , quoteDec  = stub qq "Dec" 
  }
  where
  qq = "qChr"

quoteTCharE :: String -> String -> Q Exp
quoteTCharE qq = \case
  [c] -> case toBin $ Char.ord c of
    Just bs | length bs == 8 ->
      [| TChar $(foldr (\b as -> [| $(booleanE b) :< $as |]) [|Ø|] bs) |]
    r -> fail $ qq ++ ": bad binary conversion: " ++ show r
  _   -> fail $ qq ++ ": expected Char"

quoteTCharP :: String -> String -> Q Pat
quoteTCharP qq = \case
  [c] -> case toBin $ Char.ord c of
    Just bs | length bs == 8 ->
      [p| TChar $(foldr (\b as -> [p| $(booleanP b) :< $as |]) [p|Ø|] bs) |]
    r -> fail $ qq ++ ": bad binary conversion: " ++ show r
  _   -> fail $ qq ++ ": expected Char"

quoteCharT :: String -> String -> Q Type
quoteCharT qq = \case
  [c] -> case toBin $ Char.ord c of
    Just bs | length bs == 8 ->
      [t| 'Char $(ds !! 0) $(ds !! 1) $(ds !! 2) $(ds !! 3)
                $(ds !! 4) $(ds !! 5) $(ds !! 6) $(ds !! 7)
        |]
      where
      ds        = map booleanT bs
    r -> fail $ qq ++ ": bad binary conversion: " ++ show r
  _   -> fail $ qq ++ ": expected Char"

toBin :: Int -> Maybe [Bool]
toBin n
  | n < 0 || n >= 256 = Nothing
  | otherwise         = Just $ pad 8 False $ go n []
  where
  go x ds
    | x == 0    = ds
    | otherwise = go y $ toBinDigit d : ds
    where
    (y,d) = x `divMod` 2

fromBin :: [Bool] -> Int
fromBin = foldl (\acc d -> 2 * acc + fromBinDigit d) 0

pad :: Int -> a -> [a] -> [a]
pad n a as = replicate (n - length as) a ++ as

toBinDigit :: Int -> Bool
toBinDigit = (>= 1)

fromBinDigit :: Bool -> Int
fromBinDigit b = if b then 1 else 0

booleanE :: Bool -> Q Exp
booleanE b = if b then [|True_|] else [|False_|]

booleanP :: Bool -> Q Pat
booleanP b = if b then [p|True_|] else [p|False_|]

booleanT :: Bool -> Q Type
booleanT b = if b then [t|True|] else [t|False|]

-- }}}

newtype Str = String [Chr]

type instance ('String as) == ('String bs) = as == bs

data TString :: Str -> * where
  TString :: !(Prod TChar cs)
          -> TString ('String cs)

instance Show (TString s) where
  showsPrec = showsPrec1

instance Show1 TString where
  showsPrec1 d s = showParen (d > 10)
    $ showString "TString "
    . shows (toString s)

instance TestEquality TString where
  testEquality (TString a) (TString b) = a =?= b //? qed

instance BoolEquality TString where
  boolEquality (TString a) (TString b) = a .== b

toString :: TString s -> String
toString (TString s) = toList toChar s

-- Str QQ {{{

qStr :: QuasiQuoter
qStr = QuasiQuoter
  { quoteExp  = quoteTStringE qq
  , quotePat  = quoteTStringP qq
  , quoteType = quoteStringT qq
  , quoteDec  = stub qq "Dec" 
  }
  where
  qq = "qStr"

quoteTStringE :: String -> String -> Q Exp
quoteTStringE qq s = [| TString $t |]
  where
  t = foldr (\a b -> [| $(quoteTCharE qq [a]) :< $b |]) [|Ø|] s

quoteTStringP :: String -> String -> Q Pat
quoteTStringP qq s = [p| TString $t |]
  where
  t = foldr (\a b -> [p| $(quoteTCharP qq [a]) :< $b |]) [p|Ø|] s

quoteStringT :: String -> String -> Q Type
quoteStringT qq s = [t| 'String $t |]
  where
  t = foldr (\a b -> [t| $(quoteCharT qq [a]) :< $b |]) [t|Ø|] s

-- }}}

displayQ :: Show a => Q a -> IO ()
displayQ m = do
  a <- runQ m
  print a

