{- |
Module     : Agora.Voting
Maintainer : emi@haskell.fyi
Description: Types for votes and vote counting
-}
module Agora.Voting (
  Vote (..),
) where

-- | Type representing direction of vote.
data Vote = InFavorOf | OpposedTo