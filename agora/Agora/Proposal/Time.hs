{-# LANGUAGE TemplateHaskell #-}

{- |
Module     : Agora.Proposal.Time
Maintainer : emi@haskell.fyi
Description: Time functions for proposal phases.

Time functions for proposal phases.
-}
module Agora.Proposal.Time (
  -- * Haskell-land
  ProposalTime (..),
  ProposalTimingConfig (..),

  -- * Plutarch-land
) where

import GHC.Generics qualified as GHC
import Plutus.V1.Ledger.Time (POSIXTime)
import PlutusTx qualified

-- | Represents the current time, as far as the proposal is concerned.
newtype ProposalTime = ProposalTime
  { proposalTime :: POSIXTime
  }
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
  deriving stock (Eq, Show, GHC.Generic)

data ProposalTimingConfig = ProposalTimingConfig
  { startTime :: ProposalTime
  -- ^ `S`: when the proposal was created.
  , draftTime :: ProposalTime
  -- ^ `D`: the length of the draft period.
  , votingTime :: ProposalTime
  -- ^ `V`: the length of the voting period.
  , lockingTime :: ProposalTime
  -- ^ `L`: the length of the locking period.
  , executingTime :: ProposalTime
  -- ^ `E`: the length of the execution period.
  }
  deriving stock (Eq, Show, GHC.Generic)

PlutusTx.makeIsDataIndexed ''ProposalTimingConfig [('ProposalTimingConfig, 0)]
