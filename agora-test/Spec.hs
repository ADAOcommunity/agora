--------------------------------------------------------------------------------

import Prelude

--------------------------------------------------------------------------------

import Test.Tasty (defaultMain, testGroup)

--------------------------------------------------------------------------------

-- import Spec.Model.MultiSig qualified as MultiSig
import Spec.Stake qualified as Stake
import Spec.Effect.TreasuryWithdrawalEffect qualified as TWE

-- | The Agora test suite.
main :: IO ()
main =
  defaultMain $
    testGroup
      "test suite"
      [ testGroup
          "Stake tests"
          Stake.tests
      , testGroup
          "Multisig tests"
          [ testGroup
              "MultiSig"
              [ MultiSig.plutarchTests
              , MultiSig.genTests
              ]
          ]
      , testGroup
          "Effects"
          [ testGroup
              "Treasury Withdrawal Effect"
              TWE.tests
          ]                  
      ]
