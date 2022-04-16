{-# LANGUAGE TemplateHaskell #-}

{- |
Module     : Agora.Effects.GovernorMutation
Maintainer : chfanghr@gmail.com
Description: An effect that mutates governor settings

An effect for mutating governor settings
-}
module Agora.Effects.GovernorMutation (mutateGovernorValidator, PMutateGovernorDatum (..), MutateGovernorDatum (..)) where

--------------------------------------------------------------------------------

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))
import Prelude

--------------------------------------------------------------------------------

import Plutarch (popaque)
import Plutarch.Api.V1 (
  PAddress,
  PDatum,
  PTxInfo,
  PTxOut,
  PTxOutRef,
  PValidator,
 )
import Plutarch.DataRepr (
  DerivePConstantViaData (..),
  PDataFields,
  PIsDataReprInstances (PIsDataReprInstances),
 )
import Plutarch.Monadic qualified as P

--------------------------------------------------------------------------------

import Plutus.V1.Ledger.Api (CurrencySymbol, TxOutRef)
import PlutusTx qualified

--------------------------------------------------------------------------------

import Agora.Effect (makeEffect)
import Agora.Utils (findOutputsToAddress, findTxOutDatum, passert, pfindTxInByTxOutRef, pisJust, scriptHashFromAddress)
import Plutarch.Lift (PLifted, PUnsafeLiftDecl)
import Agora.Governor (GovernorDatum, PGovernorDatum)
import Plutarch.Builtin (pforgetData)

--------------------------------------------------------------------------------

data MutateGovernorDatum = MutateGovernorDatum
  { governorRef :: TxOutRef
  , newDatum :: GovernorDatum
  }
  deriving stock (Show, GHC.Generic)
  deriving anyclass (Generic)

PlutusTx.makeIsDataIndexed ''MutateGovernorDatum[('MutateGovernorDatum, 0)]

--------------------------------------------------------------------------------

newtype PMutateGovernorDatum (s :: S)
  = PMutateGovernorDatum
      ( Term
          s
          ( PDataRecord
              '[ "governorRef" ':= PTxOutRef
               , "newDatum" ':= PGovernorDatum
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via (PIsDataReprInstances PMutateGovernorDatum)

instance PUnsafeLiftDecl PMutateGovernorDatum where type PLifted PMutateGovernorDatum = MutateGovernorDatum
deriving via (DerivePConstantViaData MutateGovernorDatum PMutateGovernorDatum) instance (PConstant MutateGovernorDatum)

--------------------------------------------------------------------------------

mutateGovernorValidator :: CurrencySymbol -> ClosedTerm PValidator
mutateGovernorValidator cs = makeEffect cs $
  \_gatCs (_datum :: Term _ PMutateGovernorDatum) _txOutRef _txInfo -> P.do
    mutation <- pletFields @'["governorRef", "newDatum"] _datum
    txInfo <- plet $ pfromData _txInfo

    let mint = pto $ pto $ pto $ pfromData $ pfield @"mint" # txInfo
    passert "Only the burning of the authority token should happen" $ plength # mint #== 1

    let govIn = mustFindGovernorInput # txInfo # mutation.governorRef
    govAddress <- plet $ pfield @"address" # govIn
    passert "Governor address should be a script address " $ pisJust #$ scriptHashFromAddress # govAddress

    let govOut = mustFindOutputToGovernor # txInfo # govAddress
        govOutDatum = pforgetData $ mustFindGovOutputDatum # txInfo # govOut
    passert "Wrong datum output to the governor" $ pforgetData mutation.newDatum #== govOutDatum

    popaque $ pconstant ()
  where
    mustFindGovernorInput :: Term s (PTxInfo :--> PTxOutRef :--> PTxOut)
    mustFindGovernorInput = phoistAcyclic $
      plam $ \info ref -> P.do
        result <- pmatch $ pfindTxInByTxOutRef # ref # info
        case result of
          PJust txOut -> pfromData $ pfield @"resolved" # txOut
          PNothing -> ptraceError "Missing input from governor script"

    mustFindOutputToGovernor :: Term s (PTxInfo :--> PAddress :--> PTxOut)
    mustFindOutputToGovernor = phoistAcyclic $
      plam $ \info address -> P.do
        outputs <- plet $ findOutputsToAddress # info # address
        passert "Require exactly one output to the governor" $ plength # outputs #== 1
        pfromData $ phead # outputs

    mustFindGovOutputDatum :: Term s (PTxInfo :--> PTxOut :--> (PAsData PDatum))
    mustFindGovOutputDatum = phoistAcyclic $
      plam $ \info out -> P.do
        datum' <- pmatch $ findTxOutDatum # info # out
        case datum' of
          PNothing -> ptraceError "Governor datum not found"
          PJust datum -> pdata $ datum
