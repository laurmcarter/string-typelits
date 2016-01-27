{-# LANGUAGE PartialTypeSignatures #-}
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

module Test where

import Data.Type.Conjunction
import Data.Type.Boolean
import Data.Type.Nat
import Data.Type.Nat.Quote
import Data.Type.Option
import Data.Type.String
import Data.Type.Product hiding (lookup')
import Type.Family.Bool
import Type.Family.List
import Type.Family.Nat
import Type.Family.Maybe

type family Lookup (x :: k) (ps :: [(k,v)]) :: Maybe v where
  Lookup x Ø              = Nothing
  Lookup x ('(y,a) :< ps) = If (x == y) (Just a) (Lookup x ps)
  
lookup' :: BoolEquality f => f x -> Prod (f :*: g) ps -> Option g (Lookup x ps)
lookup' x = \case
  Ø               -> Nothing_
  (y :*: a) :< ps -> if' (x .== y)
    (Just_ a)
    (lookup' x ps)

env :: Prod (TString :*: Nat) '[ '([qStr|x|],[qN|3|]) , '([qStr|y|],[qN|2|]) ]
env = [qStr|x|] :*: [qN|3|] :< [qStr|y|] :*: [qN|2|] :< Ø

