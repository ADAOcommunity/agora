{- |
Module     : Agora.Effects.GovernorMutation
Maintainer : chfanghr@gmail.com
Description: An effect that mutates governor settings

An effect for mutating governor settings
-}

module Agora.Effects.GovernorMutation (mutateGovernorValidator) where

--------------------------------------------------------------------------------

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))

--------------------------------------------------------------------------------

import Plutarch (popaque)
import Plutarch.Api.V1 (
  PAddress,
  PDatum,
  PMaybeData (..),
  PTxInfo,
  PTxOut,
  PTxOutRef,
  PValidator,
 )
import Plutarch.DataRepr (
  PDataFields,
  PIsDataReprInstances (PIsDataReprInstances),
 )
import Plutarch.Monadic qualified as P

--------------------------------------------------------------------------------

import Plutus.V1.Ledger.Api (CurrencySymbol)

--------------------------------------------------------------------------------

import Agora.Effect (makeEffect)
import Agora.Utils (isScriptAddress, passert, pfindDatum')

--------------------------------------------------------------------------------

newtype PMutateGovernorDatum (s :: S)
  = PMutateGovernorDatum
      ( Term
          s
          ( PDataRecord
              '[ "governorRef" ':= PTxOutRef
               , "newDatum" ':= PDatum
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via (PIsDataReprInstances PMutateGovernorDatum)

--------------------------------------------------------------------------------

mutateGovernorValidator :: CurrencySymbol -> ClosedTerm PValidator
mutateGovernorValidator cs = makeEffect cs $
  \_gatCs (_datum :: Term _ PMutateGovernorDatum) _txOutRef _txInfo -> P.do
    mutation <- pletFields @'["governorRef", "newDatum"] _datum
    txInfo <- plet $ pfromData _txInfo

    mint <- plet $ pto $ pto $ pto $ pfromData $ pfield @"mint" # txInfo
    passert "Only the burning of the authority token should happen" $ plength # mint #== 1

    govIn <- plet $ findGovernorInput # txInfo # mutation.governorRef
    govAddress <- plet $ pfield @"address" # govIn
    passert "Governor address should be a script address " $ isScriptAddress # govAddress

    govOut <- plet $ findOutputToGovernor # txInfo # govAddress
    govOutDatum <- plet $ findGovOutputDatum # txInfo # govOut

    passert "Wrong datum output to the governor" $ mutation.newDatum #== govOutDatum

    popaque $ pconstant ()
  where
    findGovernorInput :: Term s (PTxInfo :--> PTxOutRef :--> PTxOut)
    findGovernorInput = phoistAcyclic $
      plam $ \info ref' -> P.do
        ref <- plet $ pdata ref'
        inputs <- plet $ pfromData $ pfield @"inputs" # info
        result <-
          pmatch $
            pfind
              # ( plam $ \(pfromData -> inInfo) -> P.do
                    selfRef <- plet $ pfield @"outRef" # inInfo
                    selfRef #== ref
                )
              # inputs
        case result of
          PJust txOut -> pfromData $ pfield @"resolved" # txOut
          PNothing -> ptraceError "Missing input from governor script"

    findOutputToGovernor :: Term s (PTxInfo :--> PAddress :--> PTxOut)
    findOutputToGovernor = phoistAcyclic $
      plam $ \info address' -> P.do
        address <- plet $ pdata address'
        outputs <- plet $ pfromData $ pfield @"outputs" # info
        filteredOutputs <-
          plet $
            pfilter
              # ( plam $ \(pfromData -> txOut) -> P.do
                    selfAddress <- plet $ pfield @"address" # txOut
                    selfAddress #== address
                )
              # outputs
        passert "Require exactly one output to the governor" $ plength # filteredOutputs #== 1
        pfromData $ phead # filteredOutputs

    findGovOutputDatum :: Term s (PTxInfo :--> PTxOut :--> (PAsData PDatum))
    findGovOutputDatum = phoistAcyclic $
      plam $ \info out -> P.do
        datumHash' <- pmatch $ pfromData $ pfield @"datumHash" # out
        case datumHash' of
          PDJust ((pfield @"_0" #) -> datumHash) -> P.do
            datum' <- pmatch $ pfindDatum' # datumHash # info
            case datum' of
              PJust datum -> datum
              _ -> ptraceError "Governor datum not found"
          _ -> ptraceError "Missing datum"
