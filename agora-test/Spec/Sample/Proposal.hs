{- |
Module     : Spec.Sample.Proposal
Maintainer : emi@haskell.fyi
Description: Sample based testing for Proposal utxos

This module tests primarily the happy path for Proposal interactions
-}
module Spec.Sample.Proposal (
  -- * Script contexts
  proposalCreation,
  cosignProposal,
) where

--------------------------------------------------------------------------------
import Plutarch.Api.V1 (
  validatorHash,
 )
import Plutus.V1.Ledger.Api (
  Address (Address),
  Credential (ScriptCredential),
  Datum (Datum),
  PubKeyHash,
  ScriptContext (..),
  ScriptPurpose (..),
  ToData (toBuiltinData),
  TxInInfo (TxInInfo),
  TxInfo (..),
  TxOut (TxOut, txOutAddress, txOutDatumHash, txOutValue),
  TxOutRef (TxOutRef),
 )
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Value qualified as Value

--------------------------------------------------------------------------------

import Agora.Governor (
  GovernorDatum (GovernorDatum, nextProposalId, proposalThresholds),
 )
import Agora.Proposal (
  Proposal (..),
  ProposalDatum (..),
  ProposalId (..),
  ProposalStatus (..),
  ProposalVotes (..),
  ResultTag (..),
 )
import PlutusTx.AssocMap qualified as AssocMap
import Spec.Sample.Shared
import Spec.Util (datumPair, toDatumHash)

--------------------------------------------------------------------------------

-- | This script context should be a valid transaction.
proposalCreation :: ScriptContext
proposalCreation =
  let st = Value.singleton proposalPolicySymbol "" 1 -- Proposal ST
      proposalDatum :: Datum
      proposalDatum =
        Datum
          ( toBuiltinData $
              ProposalDatum
                { proposalId = ProposalId 0
                , effects =
                    AssocMap.fromList
                      [ (ResultTag 0, [])
                      , (ResultTag 1, [])
                      ]
                , status = Draft
                , cosigners = [signer]
                , thresholds = defaultProposalThresholds
                , votes = ProposalVotes AssocMap.empty
                }
          )

      govBefore :: Datum
      govBefore =
        Datum
          ( toBuiltinData $
              GovernorDatum
                { proposalThresholds = defaultProposalThresholds
                , nextProposalId = ProposalId 0
                }
          )
      govAfter :: Datum
      govAfter =
        Datum
          ( toBuiltinData $
              GovernorDatum
                { proposalThresholds = defaultProposalThresholds
                , nextProposalId = ProposalId 1
                }
          )
   in ScriptContext
        { scriptContextTxInfo =
            TxInfo
              { txInfoInputs =
                  [ TxInInfo
                      (TxOutRef "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be" 1)
                      TxOut
                        { txOutAddress = Address (ScriptCredential $ validatorHash govValidator) Nothing
                        , txOutValue = Value.assetClassValue proposal.governorSTAssetClass 1
                        , txOutDatumHash = Just (toDatumHash govBefore)
                        }
                  ]
              , txInfoOutputs =
                  [ TxOut
                      { txOutAddress = Address (ScriptCredential $ proposalValidatorHash) Nothing
                      , txOutValue =
                          mconcat
                            [ st
                            , Value.singleton "" "" 10_000_000
                            ]
                      , txOutDatumHash = Just (toDatumHash proposalDatum)
                      }
                  , TxOut
                      { txOutAddress = Address (ScriptCredential $ validatorHash govValidator) Nothing
                      , txOutValue =
                          mconcat
                            [ Value.assetClassValue proposal.governorSTAssetClass 1
                            , Value.singleton "" "" 10_000_000
                            ]
                      , txOutDatumHash = Just (toDatumHash govAfter)
                      }
                  ]
              , txInfoFee = Value.singleton "" "" 2
              , txInfoMint = st
              , txInfoDCert = []
              , txInfoWdrl = []
              , txInfoValidRange = Interval.always
              , txInfoSignatories = [signer]
              , txInfoData =
                  [ datumPair proposalDatum
                  , datumPair govBefore
                  , datumPair govAfter
                  ]
              , txInfoId = "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be"
              }
        , scriptContextPurpose = Minting proposalPolicySymbol
        }

-- | This script context should be a valid transaction.
cosignProposal :: [PubKeyHash] -> ScriptContext
cosignProposal newSigners =
  let st = Value.singleton proposalPolicySymbol "" 1 -- Proposal ST
      proposalBefore :: ProposalDatum
      proposalBefore =
        ProposalDatum
          { proposalId = ProposalId 0
          , effects =
              AssocMap.fromList
                [ (ResultTag 0, [])
                , (ResultTag 1, [])
                ]
          , status = Draft
          , cosigners = [signer]
          , thresholds = defaultProposalThresholds
          , votes = ProposalVotes AssocMap.empty
          }
      proposalAfter :: ProposalDatum
      proposalAfter = proposalBefore {cosigners = newSigners <> proposalBefore.cosigners}
      proposalRef = (TxOutRef "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be" 1)
   in ScriptContext
        { scriptContextTxInfo =
            TxInfo
              { txInfoInputs =
                  [ TxInInfo
                      proposalRef
                      TxOut
                        { txOutAddress = proposalValidatorAddress
                        , txOutValue =
                            mconcat
                              [ st
                              , Value.singleton "" "" 10_000_000
                              ]
                        , txOutDatumHash = Just (toDatumHash proposalBefore)
                        }
                  ]
              , txInfoOutputs =
                  [ TxOut
                      { txOutAddress = Address (ScriptCredential $ proposalValidatorHash) Nothing
                      , txOutValue =
                          mconcat
                            [ st
                            , Value.singleton "" "" 10_000_000
                            ]
                      , txOutDatumHash = Just (toDatumHash . Datum $ toBuiltinData proposalAfter)
                      }
                  ]
              , txInfoFee = Value.singleton "" "" 2
              , txInfoMint = st
              , txInfoDCert = []
              , txInfoWdrl = []
              , txInfoValidRange = Interval.always
              , txInfoSignatories = newSigners
              , txInfoData =
                  [ datumPair . Datum $ toBuiltinData proposalBefore
                  , datumPair . Datum $ toBuiltinData proposalAfter
                  ]
              , txInfoId = "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be"
              }
        , scriptContextPurpose = Spending proposalRef
        }
