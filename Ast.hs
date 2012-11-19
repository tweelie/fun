-- | Written by Morten Winther Olsson, (c) 2012

module Ast ( Term (..) ) where

data Term = TVar String
          | TNum Int
          | TTrue
          | TFalse
          | TPlus Term Term
          | TLeq Term Term
          | TIf Term Term Term
          | TPair Term Term
          | TFst Term
          | TSnd Term
          | TLam String Term
          | TApp Term Term
          | TLet String Term Term
          | TRec String Term
          | TCase Term String Term String Term
          | TInl Term
          | TInr Term
          deriving (Show, Eq)

