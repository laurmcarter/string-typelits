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

module Type.Family.String
  ( Chr
  , Str
  , chr
  , str
  , MkChr
  ) where

import Type.Class.Witness
import Type.Family.Bool
import Type.Family.List
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Char (ord)

-- Chr {{{

data Chr = TChar Bool Bool Bool Bool Bool Bool Bool Bool

type instance (TChar a0 a1 a2 a3 a4 a5 a6 a7)
           == (TChar b0 b1 b2 b3 b4 b5 b6 b7)
  =  a0 == b0 && a1 == b1 && a2 == b2 && a3 == b3
  && a4 == b4 && a5 == b5 && a6 == b6 && a7 == b7

chr :: QuasiQuoter
chr = QuasiQuoter
  { quoteExp  = stub qq "Exp" 
  , quotePat  = stub qq "Pat" 
  , quoteType = quoteTChar qq
  , quoteDec  = stub qq "Dec" 
  }
  where
  qq = "chr"

quoteTChar :: String -> String -> Q Type
quoteTChar qq = \case
  [c] -> case toBin $ ord c of
    Just bs | length bs == 8 ->
      [t| TChar $(ds !! 0) $(ds !! 1) $(ds !! 2) $(ds !! 3)
                $(ds !! 4) $(ds !! 5) $(ds !! 6) $(ds !! 7)
        |]
      where
      ds        = map boolLit bs
      boolLit b = if b then [t|True|] else [t|False|]
    r -> fail $ qq ++ ": bad binary conversion: " ++ show r
  _   -> fail $ qq ++ ": expected Char"

toBin :: Int -> Maybe [Bool]
toBin n
  | n < 0 || n >= 256 = Nothing
  | otherwise         = Just $ go n []
  where
  go x ds
    | x == 0    = ds
    | otherwise = go y $ toBinDigit d : ds
    where
    (y,d) = x `divMod` 2

toBinDigit :: Int -> Bool
toBinDigit = (>= 1)

-- }}}

-- Str {{{

newtype Str = TString [Chr]

type instance (TString as) == (TString bs) = as == bs

str :: QuasiQuoter
str = QuasiQuoter
  { quoteExp  = stub qq "Exp" 
  , quotePat  = stub qq "Pat" 
  , quoteType = quoteTString qq
  , quoteDec  = stub qq "Dec" 
  }
  where
  qq = "str"

quoteTString :: String -> String -> Q Type
quoteTString qq s = [t| TString $t |]
  where
  t = foldr (\a b -> [t| $(quoteTChar qq [a]) :< $b |]) [t|Ø|] s

-- }}}

stub :: String -> String -> a
stub qq fn = error $ qq ++ ": " ++ fn ++ " unprovided."

