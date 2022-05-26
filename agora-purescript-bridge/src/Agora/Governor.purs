-- File auto generated by purescript-bridge! --
module Agora.Governor where

import Prelude

import Agora.Proposal (ProposalId, ProposalThresholds)
import Agora.Proposal.Time (MaxTimeRangeWidth, ProposalTimingConfig)
import Agora.SafeMoney (GTTag)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', iso, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tagged (Tagged)
import GHC.Num.Integer (Integer)
import Plutus.V1.Ledger.Tx (TxOutRef)
import Plutus.V1.Ledger.Value (AssetClass)
import Type.Proxy (Proxy(Proxy))

newtype GovernorDatum = GovernorDatum
  { proposalThresholds :: ProposalThresholds
  , nextProposalId :: ProposalId
  , proposalTimings :: ProposalTimingConfig
  , createProposalTimeRangeMaxWidth :: MaxTimeRangeWidth
  }

derive instance Generic GovernorDatum _

derive instance Newtype GovernorDatum _

--------------------------------------------------------------------------------

_GovernorDatum :: Iso' GovernorDatum {proposalThresholds :: ProposalThresholds, nextProposalId :: ProposalId, proposalTimings :: ProposalTimingConfig, createProposalTimeRangeMaxWidth :: MaxTimeRangeWidth}
_GovernorDatum = _Newtype

--------------------------------------------------------------------------------

data GovernorRedeemer
  = CreateProposal
  | MintGATs
  | MutateGovernor

derive instance Generic GovernorRedeemer _

instance Enum GovernorRedeemer where
  succ = genericSucc
  pred = genericPred

instance Bounded GovernorRedeemer where
  bottom = genericBottom
  top = genericTop

--------------------------------------------------------------------------------

_CreateProposal :: Prism' GovernorRedeemer Unit
_CreateProposal = prism' (const CreateProposal) case _ of
  CreateProposal -> Just unit
  _ -> Nothing

_MintGATs :: Prism' GovernorRedeemer Unit
_MintGATs = prism' (const MintGATs) case _ of
  MintGATs -> Just unit
  _ -> Nothing

_MutateGovernor :: Prism' GovernorRedeemer Unit
_MutateGovernor = prism' (const MutateGovernor) case _ of
  MutateGovernor -> Just unit
  _ -> Nothing

--------------------------------------------------------------------------------

newtype Governor = Governor
  { gstOutRef :: TxOutRef
  , gtClassRef :: Tagged GTTag AssetClass
  , maximumCosigners :: Integer
  }

derive instance Generic Governor _

derive instance Newtype Governor _

--------------------------------------------------------------------------------

_Governor :: Iso' Governor {gstOutRef :: TxOutRef, gtClassRef :: Tagged GTTag AssetClass, maximumCosigners :: Integer}
_Governor = _Newtype
