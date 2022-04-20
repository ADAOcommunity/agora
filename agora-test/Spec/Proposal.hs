{-# LANGUAGE QuasiQuotes #-}

{- |
Module     : Spec.Proposal
Maintainer : emi@haskell.fyi
Description: Tests for Proposal policy and validator

Tests for Proposal policy and validator
-}
module Spec.Proposal (tests) where

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

import Agora.Proposal (
  ProposalDatum (ProposalDatum),
  ProposalId (ProposalId),
  ProposalRedeemer (Cosign),
  ProposalStatus (Draft),
  ProposalVotes (ProposalVotes),
  ResultTag (ResultTag),
  cosigners,
  effects,
  proposalId,
  proposalPolicy,
  proposalValidator,
  status,
  thresholds,
  votes,
 )
import Agora.Stake (StakeDatum (StakeDatum), StakeRedeemer (WitnessStake), stakeValidator)
import Plutarch.SafeMoney (Tagged (Tagged))
import PlutusTx.AssocMap qualified as AssocMap
import Spec.Sample.Proposal qualified as Proposal
import Spec.Sample.Shared (signer, signer2)
import Spec.Sample.Shared qualified as Shared
import Spec.Util (policySucceedsWith, validatorSucceedsWith)
import Test.Tasty (TestTree, testGroup)

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- | Stake tests.
tests :: [TestTree]
tests =
  [ testGroup
      "policy"
      [ policySucceedsWith
          "proposalCreation"
          (proposalPolicy Shared.proposal)
          ()
          Proposal.proposalCreation
      ]
  , testGroup
      "validator"
      [ testGroup
          "cosignature"
          [ validatorSucceedsWith
              "proposal"
              (proposalValidator Shared.proposal)
              ( ProposalDatum
                  { proposalId = ProposalId 0
                  , effects =
                      AssocMap.fromList
                        [ (ResultTag 0, [])
                        , (ResultTag 1, [])
                        ]
                  , status = Draft
                  , cosigners = [signer]
                  , thresholds = Shared.defaultProposalThresholds
                  , votes = ProposalVotes AssocMap.empty
                  }
              )
              (Cosign [signer2])
              (Proposal.cosignProposal [signer2])
          , validatorSucceedsWith
              "stake"
              (stakeValidator Shared.stake)
              (StakeDatum (Tagged 50_000_000) signer2 [])
              WitnessStake
              (Proposal.cosignProposal [signer2])
          ]
      ]
  ]
