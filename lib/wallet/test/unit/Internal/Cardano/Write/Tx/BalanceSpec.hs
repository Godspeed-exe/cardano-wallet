{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{- HLINT ignore "Use null" -}
{- HLINT ignore "Use camelCase" -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- TODO: https://cardanofoundation.atlassian.net/browse/ADP-2841
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 902
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
#endif

module Internal.Cardano.Write.Tx.BalanceSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Script
    ( KeyHash (..)
    , KeyRole (Policy)
    )
import Cardano.Api
    ( CardanoEra (..)
    , InAnyCardanoEra (..)
    , IsCardanoEra (..)
    )
import Cardano.Api.Gen
    ( genAddressByron
    , genAddressInEra
    , genEncodingBoundaryLovelace
    , genNetworkId
    , genPaymentCredential
    , genSignedValue
    , genStakeAddressReference
    , genTxForBalancing
    , genTxOut
    , genTxOutDatum
    , genTxOutValue
    , genValueForTxOut
    )
import Cardano.Api.Shelley
    ( fromShelleyLovelace
    )
import Cardano.Binary
    ( ToCBOR
    , serialize'
    , unsafeDeserialize'
    )
import Cardano.Ledger.Alonzo.TxInfo
    ( TranslationError (..)
    )
import Cardano.Ledger.Api
    ( AllegraEraTxBody (..)
    , AlonzoEraTxBody (..)
    , EraTx (witsTxL)
    , EraTxBody (..)
    , EraTxWits (bootAddrTxWitsL, scriptTxWitsL)
    , MaryEraTxBody (..)
    , ShelleyEraTxBody (..)
    , ValidityInterval (..)
    , allInputsTxBodyF
    , bodyTxL
    , ppCoinsPerUTxOByteL
    , ppMaxTxSizeL
    , ppMinFeeAL
    , serialiseAddr
    )
import Cardano.Ledger.Era
    ( Era
    )
import Cardano.Ledger.Language
    ( Language (..)
    )
import Cardano.Ledger.Shelley.API
    ( StrictMaybe (SJust, SNothing)
    , Withdrawals (..)
    )
import Cardano.Mnemonic
    ( SomeMnemonic (SomeMnemonic)
    , entropyToMnemonic
    , mkEntropy
    )
import Cardano.Numeric.Util
    ( power
    )
import Cardano.Pool.Types
    ( PoolId (..)
    )
import Cardano.Wallet
    ( defaultChangeAddressGen
    )
import Cardano.Wallet.Address.Derivation
    ( DelegationAddress (delegationAddress)
    , Depth (..)
    , DerivationType (..)
    , Index
    , Role (..)
    , hex
    , paymentAddress
    )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey
    )
import Cardano.Wallet.Address.Discovery.Random
    ( RndState
    , mkRndState
    )
import Cardano.Wallet.Address.Discovery.Sequential
    ( SeqState
    , defaultAddressPoolGap
    , purposeBIP44
    , purposeCIP1852
    )
import Cardano.Wallet.Address.Keys.SequentialAny
    ( mkSeqStateFromRootXPrv
    )
import Cardano.Wallet.Address.Keys.WalletKey
    ( getRawKey
    , publicKey
    )
import Cardano.Wallet.Flavor
    ( KeyFlavorS (..)
    )
import Cardano.Wallet.Primitive.Model
    ( Wallet (..)
    , unsafeInitWallet
    )
import Cardano.Wallet.Primitive.NetworkId
    ( NetworkDiscriminant (..)
    , NetworkId (..)
    , SNetworkId (..)
    , withSNetworkId
    )
import Cardano.Wallet.Primitive.Passphrase
    ( Passphrase (..)
    )
import Cardano.Wallet.Primitive.Slotting
    ( PastHorizonException
    )
import Cardano.Wallet.Primitive.Types.Credentials
    ( RootCredentials (..)
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx (..)
    , cardanoTxIdeallyNoLaterThan
    , sealedTxFromBytes
    , sealedTxFromCardano
    , sealedTxFromCardano'
    , serialisedTx
    )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( TxSize (..)
    )
import Cardano.Wallet.Shelley.Transaction
    ( mkByronWitness
    , mkDelegationCertificates
    , _decodeSealedTx
    )
import Cardano.Wallet.Transaction
    ( DelegationAction (..)
    , WitnessCountCtx (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex
    )
import Control.Lens
    ( set
    , (%~)
    , (.~)
    , (^.)
    )
import Control.Monad
    ( forM
    , forM_
    , replicateM
    )
import Control.Monad.Random
    ( evalRand
    )
import Control.Monad.Trans.Except
    ( runExcept
    , runExceptT
    )
import Control.Monad.Trans.State.Strict
    ( evalState
    , state
    )
import Data.ByteArray.Encoding
    ( Base (Base16)
    , convertToBase
    )
import Data.ByteString
    ( ByteString
    )
import Data.Char
    ( isDigit
    )
import Data.Default
    ( Default (..)
    )
import Data.Either
    ( isLeft
    , isRight
    )
import Data.Function
    ( (&)
    )
import Data.Functor.Identity
    ( Identity
    )
import Data.Generics.Internal.VL.Lens
    ( over
    , view
    )
import Data.IntCast
    ( intCast
    )
import Data.List
    ( isSuffixOf
    , sortOn
    )
import Data.List.NonEmpty
    ( NonEmpty (..)
    )
import Data.Maybe
    ( catMaybes
    , fromJust
    , fromMaybe
    )
import Data.Monoid.Monus
    ( Monus ((<\>))
    )
import Data.Ratio
    ( (%)
    )
import Data.Set
    ( Set
    )
import Data.SOP.Counting
    ( exactlyOne
    )
import Data.Text
    ( Text
    )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime
    )
import Data.Word
    ( Word8
    )
import Fmt
    ( Buildable (..)
    , blockListF
    , blockListF'
    , fmt
    , nameF
    , pretty
    )
import GHC.Stack
    ( HasCallStack
    )
import Internal.Cardano.Write.Tx
    ( AnyRecentEra (..)
    , Datum (..)
    , FeePerByte (..)
    , InAnyRecentEra (..)
    , IsRecentEra (..)
    , RecentEra (..)
    , ShelleyLedgerEra
    , Tx
    , TxIn
    , TxOut
    , TxOutInRecentEra (..)
    , TxOutInRecentEra (..)
    , UTxO
    , fromCardanoTx
    , fromCardanoUTxO
    , recentEra
    , toCardanoTx
    , toCardanoUTxO
    , utxoFromTxOutsInRecentEra
    , withConstraints
    )
import Internal.Cardano.Write.Tx.Balance
    ( ChangeAddressGen (..)
    , ErrAssignRedeemers (..)
    , ErrBalanceTx (..)
    , ErrBalanceTxInternalError (..)
    , ErrBalanceTxOutputError (..)
    , ErrBalanceTxOutputErrorInfo (..)
    , ErrMoreSurplusNeeded (..)
    , ErrUpdateSealedTx (..)
    , PartialTx (..)
    , Redeemer (..)
    , TxFeeAndChange (..)
    , TxFeeUpdate (UseNewTxFee)
    , TxUpdate (TxUpdate)
    , UTxOAssumptions (..)
    , balanceTransaction
    , constructUTxOIndex
    , costOfIncreasingCoin
    , distributeSurplus
    , distributeSurplusDelta
    , fromWalletUTxO
    , maximumCostOfIncreasingCoin
    , noTxUpdate
    , posAndNegFromCardanoValue
    , sizeOfCoin
    , updateTx
    )
import Internal.Cardano.Write.Tx.Sign
    ( KeyWitnessCount (..)
    , estimateKeyWitnessCount
    , estimateSignedTxSize
    )
import Internal.Cardano.Write.Tx.SizeEstimation
    ( sizeOf_BootstrapWitnesses
    )
import Internal.Cardano.Write.Tx.TimeTranslation
    ( TimeTranslation
    , timeTranslationFromEpochInfo
    )
import Numeric.Natural
    ( Natural
    )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
    ( RelativeTime (..)
    , mkSlotLength
    )
import Ouroboros.Consensus.Config
    ( SecurityParam (..)
    )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardBabbage
    )
import Ouroboros.Network.Block
    ( SlotNo (..)
    )
import System.Directory
    ( listDirectory
    )
import System.FilePath
    ( takeExtension
    , (</>)
    )
import System.Random.StdGenSeed
    ( StdGenSeed (..)
    , stdGenFromSeed
    )
import Test.Hspec
    ( Spec
    , describe
    , expectationFailure
    , it
    , pendingWith
    , runIO
    , shouldBe
    )
import Test.Hspec.Core.Spec
    ( SpecM
    )
import Test.Hspec.Golden
    ( Golden (..)
    )
import Test.Hspec.QuickCheck
    ( prop
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Arbitrary2 (liftShrink2)
    , Property
    , Testable
    , arbitraryBoundedEnum
    , arbitrarySizedNatural
    , checkCoverage
    , classify
    , conjoin
    , counterexample
    , cover
    , elements
    , forAll
    , frequency
    , label
    , listOf
    , oneof
    , property
    , scale
    , shrinkBoundedEnum
    , shrinkList
    , shrinkMapBy
    , tabulate
    , vectorOf
    , withMaxSuccess
    , (===)
    , (==>)
    )
import Test.QuickCheck.Extra
    ( report
    , shrinkNatural
    , (.>=.)
    )
import Test.QuickCheck.Gen
    ( Gen (..)
    )
import Test.Utils.Paths
    ( getTestData
    )
import Test.Utils.Pretty
    ( Pretty (..)
    )
import Text.Read
    ( readMaybe
    )

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Ledger.Alonzo.Core as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWits as Alonzo
import qualified Cardano.Ledger.Babbage as Babbage
import qualified Cardano.Ledger.Babbage.Core as Babbage
import qualified Cardano.Ledger.Babbage.Core as Ledger
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Val as Value
import qualified Cardano.Slotting.EpochInfo as Slotting
import qualified Cardano.Slotting.Slot as Slotting
import qualified Cardano.Slotting.Time as Slotting
import qualified Cardano.Wallet.Address.Derivation.Byron as Byron
import qualified Cardano.Wallet.Address.Derivation.Shelley as Shelley
import qualified Cardano.Wallet.Primitive.Types as W
    ( Block (..)
    , BlockHeader (..)
    )
import qualified Cardano.Wallet.Primitive.Types as W.Block
    ( header
    )
import qualified Cardano.Wallet.Primitive.Types.Address as W
    ( Address (..)
    )
import qualified Cardano.Wallet.Primitive.Types.Coin as W.Coin
import qualified Cardano.Wallet.Primitive.Types.Coin as W
    ( Coin (..)
    )
import qualified Cardano.Wallet.Primitive.Types.Coin.Gen as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
    ( Hash (..)
    , mockHash
    )
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as W.TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as W
    ( TokenBundle
    )
import qualified Cardano.Wallet.Primitive.Types.TokenBundle.Gen as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxIn as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxIn.Gen as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W.TxOut
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W
    ( TxOut (..)
    )
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut.Gen as TxOutGen
import qualified Cardano.Wallet.Primitive.Types.UTxO as W
import qualified Cardano.Wallet.Shelley.Compatibility.Ledger as Convert
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Quantity as W
    ( Quantity (..)
    )
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Internal.Cardano.Write.ProtocolParameters as Write
import qualified Internal.Cardano.Write.Tx as Write
import qualified Ouroboros.Consensus.HardFork.History as HF
import qualified Test.Hspec.Extra as Hspec

--------------------------------------------------------------------------------
-- Specifications
--------------------------------------------------------------------------------

spec :: Spec
spec = do
    spec_balanceTransaction
    spec_distributeSurplus
    spec_estimateSignedTxSize
    spec_updateTx

spec_balanceTransaction :: Spec
spec_balanceTransaction = describe "balanceTransaction" $ do
    -- TODO: Create a test to show that datums are passed through...

    it "doesn't balance transactions with existing 'totalCollateral'"
        $ property prop_balanceTransactionExistingTotalCollateral

    it "doesn't balance transactions with existing 'returnCollateral'"
        $ property prop_balanceTransactionExistingReturnCollateral

    it "produces valid transactions or fails"
        $ property prop_balanceTransactionValid

    describe "bootstrap witnesses" $ do
        -- Used in 'estimateTxSize', and in turn used by coin-selection
        let coinSelectionEstimatedSize :: Natural -> Natural
            coinSelectionEstimatedSize = unTxSize . sizeOf_BootstrapWitnesses

        let measuredWitSize :: IsCardanoEra era => Cardano.Tx era -> Natural
            measuredWitSize (Cardano.Tx body wits) = fromIntegral
                $ serializedSize (Cardano.Tx body wits)
                - serializedSize (Cardano.Tx body [])

        let evaluateMinimumFeeSize
                :: forall era. IsRecentEra era
                => Cardano.Tx era
                -> Natural
            evaluateMinimumFeeSize (Cardano.Tx body wits) = fromIntegral
                $ Write.unCoin
                $ Write.evaluateMinimumFee
                    (recentEra @era)
                    pp
                    (fromCardanoTx (Cardano.Tx body []))
                    (KeyWitnessCount 0 (fromIntegral $ length wits))
              where
                -- Dummy PParams to ensure a Coin-delta corresponds to a
                -- size-delta.
                pp = withConstraints (recentEra @era) $
                    Ledger.emptyPParams & set ppMinFeeAL (Ledger.Coin 1)

        let evaluateMinimumFeeDerivedWitSize (Cardano.Tx body wits)
                = evaluateMinimumFeeSize (Cardano.Tx body wits)
                - evaluateMinimumFeeSize (Cardano.Tx body [])

        it "coin-selection's size estimation == balanceTx's size estimation"
            $ property
            $ prop_bootstrapWitnesses
            $ \n (InAnyRecentEra _era tx) -> do
                let balanceSize = evaluateMinimumFeeDerivedWitSize tx
                let csSize = coinSelectionEstimatedSize $ intCast n
                balanceSize === csSize
                -- >= would suffice, but we can be stronger

        it "balanceTx's size estimation >= measured serialized size"
            $ property
            $ prop_bootstrapWitnesses
            $ \n (InAnyRecentEra _era tx) -> do
                let estimated = evaluateMinimumFeeDerivedWitSize tx
                let measured = measuredWitSize tx
                let overestimation
                        | estimated > measured = estimated - measured
                        | otherwise            = 0

                let tabulateOverestimation = tabulate "overestimation/wit" $
                        if n == 0
                        then [show overestimation <> " (but with no wits)"]
                        else [show $ overestimation `div` fromIntegral n]

                estimated .>=. measured
                    & tabulateOverestimation

    balanceTransactionGoldenSpec

    describe "posAndNegFromCardanoValue" $
        it "roundtrips with toCardanoValue" $
            property prop_posAndNegFromCardanoValueRoundtrip

    describe "change address generation" $ do
        let balance' =
                balanceTransactionWithDummyChangeState
                    AllKeyPaymentCredentials
                    dustUTxO
                    testStdGenSeed

        -- We could generate arbitrary tx bodies to test with, but by
        -- limiting ourselves to 'paymentPartialTx' with a fixed number of
        -- payments 1) ensures balancing always succeeds 2) makes it easy to
        -- have separate 'it' statements for different expectations of the same
        -- test case.
        let nPayments = 10
        let paymentOuts = replicate nPayments $
                W.TxOut
                    dummyAddr
                    (W.TokenBundle.fromCoin (W.Coin 1_000_000))
        let ptx = paymentPartialTx paymentOuts

        -- True for values of nPayments small enough not to cause
        -- 'ErrBalanceTxMaxSizeLimitExceeded' or ErrMakeChange
        let nChange = max nPayments 1
        let s0 = DummyChangeState 0
        let expectedChange = fmap Convert.toWalletAddress <$>
                flip evalState s0
                $ replicateM nChange
                $ state @Identity (getChangeAddressGen dummyChangeAddrGen)

        let address :: Babbage.BabbageTxOut StandardBabbage -> W.Address
            address (Babbage.BabbageTxOut addr _ _ _) = Convert.toWallet addr

        let (tx, s') =
                either (error . show) id $ balance' ptx

        it "assigns change addresses as expected" $
            map address (outputs tx)
                `shouldBe`
                (map (view #address) paymentOuts ++ expectedChange)

        it "returns s' corresponding to which addresses were used" $ do
            s' `shouldBe` DummyChangeState { nextUnusedIndex = nChange }

    it "assigns minimal ada quantities to outputs without ada" $ do
        let era = RecentEraBabbage
        let out = W.TxOut dummyAddr (W.TokenBundle.fromCoin (W.Coin 0))
        let out' = W.TxOut dummyAddr (W.TokenBundle.fromCoin (W.Coin 874_930))
        let Cardano.ShelleyTx _ tx = either (error . show) id
                $ balance
                $ paymentPartialTx [ out ]
        let outs = Write.outputs era $ Write.txBody era tx

        let pp = def
                & ppCoinsPerUTxOByteL .~ testParameter_coinsPerUTxOByte_Babbage
        Write.isBelowMinimumCoinForTxOut era pp (head outs)
            `shouldBe` False

        head outs `shouldBe` (Convert.toBabbageTxOut out')

    describe "effect of txMaxSize on coin selection" $ do

        let balanceWithDust = balanceTx
                dustWallet
                mockPParamsForBalancing
                dummyTimeTranslation
                testStdGenSeed

        let totalOutput tx =
                let (wtx, _, _, _, _, _) =
                        _decodeSealedTx maxBound
                        (ShelleyWalletCtx dummyPolicyK)
                        (sealedTxFromCardano' tx)
                in
                    F.foldMap (view (#tokens . #coin)) (view #outputs wtx)
                    <> fromMaybe (W.Coin 0) (view #fee wtx)

        it "tries to select 2x the payment amount" $ do
            let tx = balanceWithDust $ paymentPartialTx
                    [ W.TxOut dummyAddr
                        (W.TokenBundle.fromCoin (W.Coin 50_000_000))
                    ]
            totalOutput <$> tx `shouldBe` Right (W.Coin 100_000_000)

        it "falls back to 1x if out of space" $ do
            let tx = balanceWithDust $ paymentPartialTx
                    [ W.TxOut dummyAddr
                        (W.TokenBundle.fromCoin (W.Coin 100_000_000))
                    ]
            totalOutput <$> tx `shouldBe` Right (W.Coin 102_000_000)

        it "otherwise fails with ErrBalanceTxMaxSizeLimitExceeded" $ do
            let tx = balanceWithDust $ paymentPartialTx
                    [ W.TxOut dummyAddr
                        (W.TokenBundle.fromCoin (W.Coin 200_000_000))
                    ]
            tx `shouldBe` Left ErrBalanceTxMaxSizeLimitExceeded

    describe "when passed unresolved inputs" $ do
        it "fails with ErrBalanceTxUnresolvedTxIn" $ do
            let txin = W.TxIn (W.Hash $ B8.replicate 32 '3') 10

            -- 1 output, 1 input without utxo entry
            let partialTx :: PartialTx Cardano.BabbageEra
                partialTx = addExtraTxIns [txin] $
                    paymentPartialTx
                        [ W.TxOut dummyAddr
                            (W.TokenBundle.fromCoin (W.Coin 1_000_000))
                        ]
            balance partialTx
                `shouldBe`
                Left (ErrBalanceTxUnresolvedInputs (txin :| []))

        describe "with redeemers" $
            it "fails with ErrBalanceTxUnresolvedInputs" $ do
                let withNoUTxO :: PartialTx era -> PartialTx era
                    withNoUTxO ptx = ptx { inputs = mempty }

                balance (withNoUTxO pingPong_2)
                    `shouldBe` Left
                        (ErrBalanceTxUnresolvedInputs $ NE.fromList
                            [ W.TxIn
                                (W.Hash "11111111111111111111111111111111") 0
                            ]
                        )

    describe "when validity interval is too far in the future" $ do
        let withValidityBeyondHorizon = withValidityInterval
                $ ValidityInterval SNothing (SJust beyondHorizon)
        describe "with some Plutus redeemers" $ do
            it "fails with TimeTranslationPastHorizon" $ do
                case balance (withValidityBeyondHorizon pingPong_2) of
                    Left
                        (ErrBalanceTxAssignRedeemers
                            (ErrAssignRedeemersTranslationError
                                (TimeTranslationPastHorizon
                                    _pastHoriozon))) -> return ()
                    other -> expectationFailure $
                        "Expected pastHorizon failure; got " <> show other

        describe "with no redeemers" $ do
            it "succeeds at balancing" $ do
                case balance (withValidityBeyondHorizon pingPong_1) of
                    Right _tx -> return ()
                    other -> expectationFailure $
                        "Expected (Right tx); got " <> show other

    describe "when a redeemer is missing" $ do
        it "balancing succeeds (currently)" $ do
            -- Plutus script inputs must have a corresponding redeemer. If one
            -- is missing, the resuling tx will not be acceptable by the ledger.
            -- Instead of the current behaviour, it might make more sense to
            -- fail.
            --
            -- The only reason for succeeding might be if a party, say Bob, only
            -- wants to add a redeemer, revealing the redeemer value, after
            -- seeing Alice partially balance the transaction. However it seems
            -- too unclear both whether 1) this is actually is useful, and
            -- 2) whether it would work techincally, aside from lack of
            -- protective guard in balanceTx, so failing might still be saner.
            let withNoRedeemers = over #redeemers (const [])
            case balance (withNoRedeemers pingPong_2) of
                Right _tx -> pure ()
                other -> expectationFailure $
                    "Expected (Right tx); got " <> show other

    describe "when a redeemer points to an input that doesn't exist" $ do
        it "fails with ErrAssignRedeemersTargetNotFound" $ do

            let tid = W.Hash $ B8.replicate 32 '1'

            -- With ix 1 instead of 0, making it point to an input which
            -- doesn't exist in the tx.
            let faultyRedeemer =
                    RedeemerSpending (unsafeFromHex "D87A80") (W.TxIn tid 1)

            let withFaultyRedeemer =
                    over #redeemers $ mapFirst $ const faultyRedeemer

            balance (withFaultyRedeemer pingPong_2)
                `shouldBe`
                Left (ErrBalanceTxAssignRedeemers
                        (ErrAssignRedeemersTargetNotFound faultyRedeemer))
  where
    outputs
        :: Cardano.Tx Cardano.BabbageEra
        -> [Babbage.BabbageTxOut StandardBabbage]
    outputs (Cardano.Tx (Cardano.ShelleyTxBody _ body _ _ _ _ ) _) =
        Write.outputs RecentEraBabbage body

    mapFirst f (x:xs) = f x : xs
    mapFirst _ [] = error "mapFirst: empty list"

    horizon = SlotNo 20
    beyondHorizon = SlotNo 21

    wallet = mkTestWallet (utxo [W.Coin 5_000_000])

    -- Wallet with only small utxos, and enough of them to fill a tx in the
    -- tests below.
    dustWallet = mkTestWallet dustUTxO
    dustUTxO = W.UTxO $ Map.fromList $
        [ ( W.TxIn (W.Hash $ B8.replicate 32 '1') ix
          , W.TxOut dummyAddr (W.TokenBundle.fromCoin $ W.Coin 1_000_000)
          )
        | ix <- [0 .. 500]
        ]

    balance = balanceTx
        wallet
        mockPParamsForBalancing
        (dummyTimeTranslationWithHorizon horizon)
        testStdGenSeed

    utxoWithBundles bundles = W.UTxO $ Map.fromList $ zip ins outs
      where
        ins = map (W.TxIn dummyHash) [0..]
        outs = map (W.TxOut dummyAddr) bundles
        dummyHash = W.Hash $ B8.replicate 32 '0'

    utxo coins = utxoWithBundles $ map W.TokenBundle.fromCoin coins

    dummyAddr = W.Address $ unsafeFromHex
        "60b1e5e0fb74c86c801f646841e07cdb42df8b82ef3ce4e57cb5412e77"

balanceTransactionGoldenSpec :: Spec
balanceTransactionGoldenSpec = describe "balance goldens" $ do
    it "testPParams" $
        let name = "testPParams"
            dir = $(getTestData) </> "balanceTx" </> "binary"
            ledgerPParams = Write.pparamsLedger
                $ mockPParamsForBalancing @Cardano.BabbageEra
        in Golden
            { output = ledgerPParams
            , encodePretty = show
            , writeToFile = \fp -> T.writeFile fp . T.pack . toCBORHex
            , readFromFile =
                (unsafeDeserialize' . unsafeFromHex . B8.pack <$>) . readFile
            , goldenFile = dir </> name </> "golden"
            , actualFile = Just (dir </> name </> "actual")
            , failFirstTime = False
            }

    describe "balanced binaries" $ do
        let dir = $(getTestData) </> "balanceTx" </> "binary" </> "balanced"
        let walletUTxO = utxo [W.Coin 5_000_000]
        it "pingPong_2" $ do
            let ptx = pingPong_2
            let tx = either (error . show) id $ balanceTx
                    (mkTestWallet walletUTxO)
                    mockPParamsForBalancing
                    dummyTimeTranslation
                    testStdGenSeed
                    ptx
            let serializeTx = serialisedTx
                    . sealedTxFromCardano
                    . InAnyCardanoEra Cardano.BabbageEra

            let name = "pingPong_2"
            Golden
                { output = tx
                , encodePretty = show
                , writeToFile = \fp x ->
                    T.writeFile fp $ T.pack . B8.unpack . hex $ serializeTx x
                , readFromFile =
                    fmap (deserializeBabbageTx . unsafeFromHex . B8.pack)
                    . readFile
                , goldenFile = dir </> name </> "golden"
                , actualFile = Just (dir </> name </> "actual")
                , failFirstTime = False
                }

    describe "results when varying wallet balance (1 UTxO)" $ do
        test "pingPong_1" pingPong_1
        test "pingPong_2" pingPong_2
        test "delegate" delegate
        test "1ada-payment" payment
  where
    toCBORHex :: ToCBOR a => a -> String
    toCBORHex = B8.unpack . hex . serialize'

    test :: String -> PartialTx Cardano.BabbageEra -> Spec
    test name partialTx = it name $ do
        goldenText name
            (map (mkGolden partialTx . W.Coin) defaultWalletBalanceRange)
      where
        defaultWalletBalanceRange = [0, 50_000 .. 4_000_000]

        goldenText :: String -> [BalanceTxGolden] -> Golden Text
        goldenText name' res =
            Golden
                { output = fmt $ blockListF' "" build res
                , encodePretty = T.unpack
                , writeToFile = T.writeFile
                , readFromFile = T.readFile
                , goldenFile = dir </> name' </> "golden"
                , actualFile = Just (dir </> name' </> "actual")
                , failFirstTime = False
                }
          where
            dir = $(getTestData) </> "balanceTx"

        mkGolden :: PartialTx Cardano.BabbageEra -> W.Coin -> BalanceTxGolden
        mkGolden ptx c =
            let
                walletUTxO = utxo [c]
                res = balanceTx
                    (mkTestWallet walletUTxO)
                    mockPParamsForBalancing
                    dummyTimeTranslation
                    testStdGenSeed
                    ptx
                combinedUTxO = mconcat
                        [ view #inputs ptx
                        , toCardanoUTxO $ fromWalletUTxO
                            RecentEraBabbage
                            walletUTxO
                        ]
            in
                case res of
                    Right tx
                        -> BalanceTxGoldenSuccess c
                                (txFee tx)
                                (txMinFee tx combinedUTxO)
                    Left e
                        -> BalanceTxGoldenFailure c (show e)

    utxo coins = W.UTxO $ Map.fromList $ zip ins outs
      where
        ins = map (W.TxIn dummyHash) [0..]
        outs = map (W.TxOut addr . W.TokenBundle.fromCoin) coins
        dummyHash = W.Hash $ B8.replicate 32 '0'

    rootK =
        Shelley.unsafeGenerateKeyFromSeed (dummyMnemonic, Nothing) mempty
    addr = W.Address $ unsafeFromHex
        "60b1e5e0fb74c86c801f646841e07cdb42df8b82ef3ce4e57cb5412e77"

    payment :: PartialTx Cardano.BabbageEra
    payment = paymentPartialTx
        [ W.TxOut addr (W.TokenBundle.fromCoin (W.Coin 1_000_000))
        ]

    delegate :: PartialTx Cardano.BabbageEra
    delegate = PartialTx (Cardano.Tx body []) mempty []
      where
        body = Cardano.ShelleyTxBody
            Cardano.ShelleyBasedEraBabbage
            (Ledger.mkBasicTxBody & certsTxBodyL .~ StrictSeq.fromList certs)
            []
            Cardano.TxBodyNoScriptData
            Nothing
            Cardano.TxScriptValidityNone

        certs =
            Cardano.toShelleyCertificate
                <$> mkDelegationCertificates delegationAction (Left xpub)
          where
            poolId = PoolId "\236(\243=\203\230\214@\n\RS^3\155\208d|\
                            \\ts\202l\f\249\194\187\230\131\141\198"

            xpub = getRawKey ShelleyKeyS $ publicKey ShelleyKeyS rootK
            delegationAction = JoinRegisteringKey poolId

    txFee :: Cardano.Tx Cardano.BabbageEra -> Cardano.Lovelace
    txFee (Cardano.Tx (Cardano.TxBody content) _) =
        case Cardano.txFee content of
            Cardano.TxFeeExplicit _ c -> c
            Cardano.TxFeeImplicit _ -> error "implicit fee"

spec_distributeSurplus :: Spec
spec_distributeSurplus = describe "distributeSurplus" $ do
    describe "sizeOfCoin" $ do
        let coinToWord64Clamped = fromMaybe maxBound . W.Coin.toWord64Maybe
        let cborSizeOfCoin =
                TxSize
                . fromIntegral
                . BS.length
                . CBOR.toStrictByteString
                . CBOR.encodeWord64 . coinToWord64Clamped

        let isBoundary c =
                sizeOfCoin c /= sizeOfCoin (c <\> W.Coin 1)
                || sizeOfCoin c /= sizeOfCoin (c <> W.Coin 1)

        it "matches the size of the Word64 CBOR encoding" $
            property $ checkCoverage $
                forAll genEncodingBoundaryLovelace $ \l -> do
                    let c = cardanoToWalletCoin l
                    let expected = cborSizeOfCoin c

                    -- Use a low coverage requirement of 0.01% just to
                    -- ensure we see /some/ amount of every size.
                    let coverSize s = cover 0.01 (s == expected) (show s)
                    sizeOfCoin c === expected
                        & coverSize (TxSize 1)
                        & coverSize (TxSize 2)
                        & coverSize (TxSize 3)
                        & coverSize (TxSize 5)
                        & coverSize (TxSize 9)
                        & cover 0.5 (isBoundary c) "boundary case"

        describe "boundary case goldens" $ do
            it "1 byte to 2 byte boundary" $ do
                sizeOfCoin (W.Coin 23) `shouldBe` TxSize 1
                sizeOfCoin (W.Coin 24) `shouldBe` TxSize 2
            it "2 byte to 3 byte boundary" $ do
                sizeOfCoin (W.Coin $ 2 `power` 8 - 1) `shouldBe` TxSize 2
                sizeOfCoin (W.Coin $ 2 `power` 8    ) `shouldBe` TxSize 3
            it "3 byte to 5 byte boundary" $ do
                sizeOfCoin (W.Coin $ 2 `power` 16 - 1) `shouldBe` TxSize 3
                sizeOfCoin (W.Coin $ 2 `power` 16    ) `shouldBe` TxSize 5
            it "5 byte to 9 byte boundary" $ do
                sizeOfCoin (W.Coin $ 2 `power` 32 - 1) `shouldBe` TxSize 5
                sizeOfCoin (W.Coin $ 2 `power` 32    ) `shouldBe` TxSize 9

    describe "costOfIncreasingCoin" $ do
        it "costs 176 lovelace to increase 4294.967295 ada (2^32 - 1 lovelace) \
           \by 1 lovelace on mainnet" $ do

            let expectedCostIncrease = W.Coin 176
            let mainnet = mainnetFeePerByte
            costOfIncreasingCoin mainnet (W.Coin $ 2 `power` 32 - 1) (W.Coin 1)
                `shouldBe` expectedCostIncrease

        it "produces results in the range [0, 8 * feePerByte]" $
            property $ \c increase -> do
                let res = costOfIncreasingCoin (FeePerByte 1) c increase
                counterexample (show res <> "out of bounds") $
                    res >= W.Coin 0 && res <= W.Coin 8

    describe "distributeSurplus" $ do

        it "prop_distributeSurplus_onSuccess_conservesSurplus" $
            prop_distributeSurplus_onSuccess_conservesSurplus
                & property
        it "prop_distributeSurplus_onSuccess_coversCostIncrease" $
            prop_distributeSurplus_onSuccess_coversCostIncrease
                & property
        it "prop_distributeSurplus_onSuccess_doesNotReduceChangeCoinValues" $
            prop_distributeSurplus_onSuccess_doesNotReduceChangeCoinValues
                & property
        it "prop_distributeSurplus_onSuccess_doesNotReduceFeeValue" $
            prop_distributeSurplus_onSuccess_doesNotReduceFeeValue
                & property
        it "prop_distributeSurplus_onSuccess_preservesChangeLength" $
            prop_distributeSurplus_onSuccess_preservesChangeLength
                & property
        it "prop_distributeSurplus_onSuccess_preservesChangeAddresses" $
            prop_distributeSurplus_onSuccess_preservesChangeAddresses
                & property
        it "prop_distributeSurplus_onSuccess_preservesChangeNonAdaAssets" $
            prop_distributeSurplus_onSuccess_preservesChangeNonAdaAssets
                & property
        it "prop_distributeSurplus_onSuccess_onlyAdjustsFirstChangeValue" $
            prop_distributeSurplus_onSuccess_onlyAdjustsFirstChangeValue
                & property
        it "prop_distributeSurplus_onSuccess_increasesValuesByDelta" $
            prop_distributeSurplus_onSuccess_increasesValuesByDelta
                & property

    describe "distributeSurplusDelta" $ do

        -- NOTE: The test values below make use of 255 being encoded as 2 bytes,
        -- and 256 as 3 bytes.

        describe "when increasing change increases fee" $
            it "will increase fee (99 lovelace for change, 1 for fee)" $
                distributeSurplusDelta
                    (FeePerByte 1)
                    (W.Coin 100)
                    (TxFeeAndChange (W.Coin 200) [W.Coin 200])
                    `shouldBe`
                    Right (TxFeeAndChange (W.Coin 1) [W.Coin 99])

        describe "when increasing fee increases fee" $
            it "will increase fee (98 lovelace for change, 2 for fee)" $ do
                distributeSurplusDelta
                    (FeePerByte 1)
                    (W.Coin 100)
                    (TxFeeAndChange (W.Coin 255) [W.Coin 200])
                    `shouldBe`
                    Right (TxFeeAndChange (W.Coin 2) [W.Coin 98])

        describe
            (unwords
                [ "when increasing the change costs more in fees than the"
                , "increase itself"
                ]) $ do
            it "will try burning the surplus as fees" $ do
                distributeSurplusDelta
                    mainnetFeePerByte
                    (W.Coin 10)
                    (TxFeeAndChange (W.Coin 200) [W.Coin 255])
                    `shouldBe`
                    Right (TxFeeAndChange (W.Coin 10) [W.Coin 0])

            it "will fail if neither the fee can be increased" $ do
                distributeSurplusDelta
                    mainnetFeePerByte
                    (W.Coin 10)
                    (TxFeeAndChange (W.Coin 255) [W.Coin 255])
                    `shouldBe`
                    Left (ErrMoreSurplusNeeded $ W.Coin 34)

        describe "when no change output is present" $ do
            it "will burn surplus as excess fees" $
                property $ \surplus fee0 -> do
                    distributeSurplusDelta
                        (FeePerByte 1)
                        surplus
                        (TxFeeAndChange fee0 [])
                        `shouldBe`
                        Right (TxFeeAndChange surplus [])

        it "prop_distributeSurplusDelta_coversCostIncreaseAndConservesSurplus" $
            prop_distributeSurplusDelta_coversCostIncreaseAndConservesSurplus
                & withMaxSuccess 10_000
                & property

spec_estimateSignedTxSize :: Spec
spec_estimateSignedTxSize = describe "estimateSignedTxSize" $ do
    txBinaries <- runIO signedTxTestData
    describe "equals the binary size of signed txs" $
        forAllGoldens txBinaries test
  where
    test
        :: forall era. IsRecentEra era
        => String
        -> ByteString
        -> Cardano.Tx era
        -> IO ()
    test _name bs cTx@(Cardano.Tx body _) = do
        let pparams = Write.pparamsLedger $ mockPParamsForBalancing @era
            witCount dummyAddr = estimateKeyWitnessCount
                (utxoPromisingInputsHaveAddress era dummyAddr tx)
                body
            era = recentEra @era

            tx :: Tx (ShelleyLedgerEra era)
            tx = fromCardanoTx @era cTx

            noScripts = withConstraints (recentEra @era)
                $ Map.null $ tx ^. witsTxL . scriptTxWitsL
            noBootWits = withConstraints (recentEra @era)
                $ Set.null $ tx ^. witsTxL . bootAddrTxWitsL
            testDoesNotYetSupport x =
                pendingWith $ "Test setup does not work for txs with " <> x

            signedBinarySize = TxSize $ fromIntegral $ BS.length bs

        case (noScripts, noBootWits) of
                (True, True) -> do
                    estimateSignedTxSize era pparams (witCount vkCredAddr) tx
                        `shouldBeInclusivelyWithin`
                        ( signedBinarySize - correction
                        , signedBinarySize
                        )
                (False, False) ->
                    testDoesNotYetSupport "bootstrap wits + scripts"
                (True, False) ->
                    estimateSignedTxSize era pparams (witCount bootAddr) tx
                        `shouldBeInclusivelyWithin`
                        ( signedBinarySize - correction
                        , signedBinarySize + bootWitsCanBeLongerBy
                        )
                (False, True) -> testDoesNotYetSupport "scripts"
      where
        -- Apparently the cbor encoding used by the ledger for size checks
        -- (`toCBORForSizeComputation`) is a few bytes smaller than the actual
        -- serialized size for these goldens.
        correction = TxSize 6

    forAllGoldens
        :: [(String, ByteString)]
        -> (forall era. IsRecentEra era
            => String
            -> ByteString
            -> Cardano.Tx era
            -> IO ())
        -> Spec
    forAllGoldens goldens f = forM_ goldens $ \(name, bs) -> it name $
        Write.withInAnyRecentEra (recentEraTxFromBytes bs) $ \tx ->
            let
                msg = unlines
                    [ B8.unpack $ hex bs
                    , pretty
                        $ sealedTxFromCardano
                        $ InAnyCardanoEra cardanoEra tx
                    ]
            in
                Hspec.counterexample msg $ f name bs tx

    -- estimateSignedTxSize now depends upon being able to resolve inputs. To
    -- keep tese tests working, we can create a UTxO with dummy values as long
    -- as estimateSignedTxSize can tell that all inputs in the tx correspond to
    -- outputs with vk payment credentials.
    utxoPromisingInputsHaveAddress
        :: forall era. HasCallStack
        => RecentEra era
        -> W.Address
        -> Tx (ShelleyLedgerEra era)
        -> UTxO (ShelleyLedgerEra era)
    utxoPromisingInputsHaveAddress era addr tx =
        utxoFromTxOutsInRecentEra era $
            [ (i
              , TxOutInRecentEra
                    (Convert.toLedger addr)
                    (Convert.toLedgerTokenBundle mempty)
                    NoDatum
                    Nothing
              )
            | i <- allInputs tx
            ]
      where
        allInputs
            :: Tx (ShelleyLedgerEra era)
            -> [TxIn]
        allInputs body = withConstraints era
            $ Set.toList
            $ body ^. (bodyTxL . allInputsTxBodyF)

    -- An address with a vk payment credential. For the test above, this is the
    -- only aspect which matters.
    vkCredAddr = W.Address $ unsafeFromHex
        "6000000000000000000000000000000000000000000000000000000000"

    -- This is a short bootstrap address retrieved from
    -- "byron-address-format.md".
    bootAddr = W.Address $ unsafeFromHex
        "82d818582183581cba970ad36654d8dd8f74274b733452ddeab9a62a397746be3c42ccdda0001a9026da5b"

    -- With more attributes, the address can be longer. This value was chosen
    -- /experimentally/ to make the tests pass. The ledger has been validating
    -- new outputs with bootstrap addresses have attributes not larger than 64
    -- bytes. The ledger has done so since the middle of the Byron era.
    -- Address attributes are included in the bootstrap witnesses.
    --
    -- NOTE: If we had access to the real UTxO set for the inputs of the test
    -- txs, we wouldn't need this fuzziness. Related: ADP-2987.
    bootWitsCanBeLongerBy = TxSize 45

spec_updateTx :: Spec
spec_updateTx = describe "updateTx" $ do
    describe "no existing key witnesses" $ do
        txs <- readTestTransactions
        forM_ txs $ \(filepath, sealedTx) -> do
            let anyRecentEraTx
                    = fromJust $ Write.asAnyRecentEra $ cardanoTx sealedTx
            it ("without TxUpdate: " <> filepath) $ do
                Write.withInAnyRecentEra anyRecentEraTx $ \tx ->
                    case updateTx tx noTxUpdate of
                        Left e ->
                            expectationFailure $
                            "expected update to succeed but failed: "
                                <> show e
                        Right tx' -> do
                            if tx /= tx' && show tx == show tx'
                            -- The transaction encoding has changed.
                            -- Unfortunately transactions are compared using
                            -- their memoized bytes, but shown without their
                            -- memoized bytes. This leads to the very
                            -- confusing situation where the show result of
                            -- two transactions is identical, but the (==)
                            -- result of two transactions shows a
                            -- discrepancy.
                            --
                            -- In this case we expect failure and write out
                            -- the new memoized bytes to a file so the
                            -- developer can update the binary test data.
                            then do
                                let
                                    newEncoding = convertToBase Base16 $
                                        Cardano.serialiseToCBOR tx'
                                    rejectFilePath
                                        = $(getTestData)
                                        </> "plutus"
                                        </> filepath <> ".rej"
                                BS.writeFile rejectFilePath newEncoding
                                expectationFailure $ mconcat
                                    [ "Transaction encoding has changed, "
                                    , "making comparison impossible. "
                                    , "See .rej file: "
                                    , rejectFilePath
                                    ]
                            else
                                Cardano.serialiseToCBOR tx
                                  `shouldBe` Cardano.serialiseToCBOR tx'

            prop ("with TxUpdate: " <> filepath) $
                prop_updateTx anyRecentEraTx

    describe "existing key witnesses" $ do

        signedTxs <- runIO signedTxTestData

        it "returns `Left err` with noTxUpdate" $ do
            -- Could be argued that it should instead return `Right tx`.
            let anyRecentEraTx = recentEraTxFromBytes
                    $ snd $ head signedTxs
            Write.withInAnyRecentEra anyRecentEraTx $ \tx ->
                updateTx tx noTxUpdate
                    `shouldBe` Left (ErrExistingKeyWitnesses 1)

        it "returns `Left err` when extra body content is non-empty" $ do
            pendingWith "todo: add test data"
  where
    readTestTransactions :: SpecM a [(FilePath, SealedTx)]
    readTestTransactions = runIO $ do
        let dir = $(getTestData) </> "plutus"
        paths <- listDirectory dir
        files <- flip foldMap paths $ \f ->
            -- Ignore reject files
            if ".rej" `isSuffixOf` takeExtension f
            then pure []
            else do
                contents <- BS.readFile (dir </> f)
                pure [(f, contents)]
        traverse (\(f,bs) -> (f,) <$> unsafeSealedTxFromHex bs) files

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

prop_balanceTransactionExistingReturnCollateral
    :: Wallet'
    -> ShowBuildable (PartialTx Cardano.BabbageEra)
    -> StdGenSeed
    -> Property
prop_balanceTransactionExistingReturnCollateral
    wallet (ShowBuildable partialTx@PartialTx{tx}) seed = withMaxSuccess 10 $
        hasReturnCollateral tx
            && not (hasInsCollateral tx)
            && not (hasTotalCollateral tx) ==>
        case balanceTx wallet pp dummyTimeTranslation seed partialTx of
            Left err -> ErrBalanceTxExistingReturnCollateral === err
            e -> counterexample (show e) False
  where
    pp = mockPParamsForBalancing

prop_balanceTransactionExistingTotalCollateral
    :: Wallet'
    -> ShowBuildable (PartialTx Cardano.BabbageEra)
    -> StdGenSeed
    -> Property
prop_balanceTransactionExistingTotalCollateral
    wallet (ShowBuildable partialTx@PartialTx{tx}) seed = withMaxSuccess 10 $
        hasTotalCollateral tx
            && not (hasInsCollateral tx)
            && not (hasReturnCollateral tx) ==>
        case balanceTx wallet pp dummyTimeTranslation seed partialTx of
            Left err -> ErrBalanceTxExistingTotalCollateral === err
            e -> counterexample (show e) False
  where
    pp = mockPParamsForBalancing

-- NOTE: 'balanceTransaction' relies on estimating the number of witnesses that
-- will be needed. The correctness of this estimation is not tested here.
--
-- TODO: Ensure scripts are well tested
--   - Ensure we have coverage for normal plutus contracts
prop_balanceTransactionValid
    :: forall era. era ~ Cardano.BabbageEra
    -- TODO [ADP-2997] Test with all RecentEras
    -- https://cardanofoundation.atlassian.net/browse/ADP-2997
    => Wallet'
    -> ShowBuildable (PartialTx Cardano.BabbageEra)
    -> StdGenSeed
    -> Property
prop_balanceTransactionValid
    wallet@(Wallet' _ walletUTxO _) (ShowBuildable partialTx) seed =
        withMaxSuccess 1_000 $ do
        let combinedUTxO =
                fromCardanoUTxO (view #inputs partialTx)
                <> fromWalletUTxO (recentEra @era) walletUTxO

        let originalBalance = txBalance (view #tx partialTx) combinedUTxO
        let originalOuts = txOutputs (view #tx partialTx)
        let classifications =
                classify (hasZeroAdaOutputs $ view #tx partialTx)
                    "partial tx had zero ada outputs"
                . classify (hasZeroAdaOutputs $ view #tx partialTx)
                    "partial tx had zero ada outputs"
                . classify (length originalOuts > 0)
                    "has payment outputs"
                . classify (length originalOuts > 5)
                    ">5 payment outputs"
                . classify (length originalOuts > 10)
                    ">10 payment outputs"
                . classify (length originalOuts > 20)
                    ">20 payment outputs"
                . classify (length originalOuts > 100)
                    ">100 payment outputs"

        let res =
                balanceTx
                    wallet
                    mockPParamsForBalancing
                    dummyTimeTranslation
                    seed
                    partialTx
        classifications $ case res of
            Right tx -> counterexample ("\nResult: " <> show (Pretty tx)) $ do
                label "success"
                    $ classify (originalBalance == mempty)
                        "already balanced"
                    $ classify (txFee tx > Cardano.Lovelace 1_000_000)
                        "fee above 1 ada"
                    $ classify (hasCollateral tx)
                        "balanced tx has collateral"
                    $ conjoin
                        [ txBalance tx combinedUTxO === mempty
                        , prop_validSize tx combinedUTxO
                        , prop_minfeeIsCovered tx combinedUTxO
                        , let
                              minUTxOValue = Cardano.Lovelace 999_978
                              upperBoundCostOfOutput = Cardano.Lovelace 1_000
                          in
                              -- Coin selection should only pay more fees than
                              -- required when it can't afford to create a
                              -- change output with the minimumUTxOValue.
                              prop_expectFeeExcessSmallerThan
                                  (minUTxOValue <> upperBoundCostOfOutput)
                                  tx
                                  combinedUTxO

                        -- FIXME [ADP-2419] Re-enable when we have stricter
                        -- validation. Will otherwise fail with:
                        --
                        -- @
                        --     --match balanceTransaction --seed 139473932`
                        -- @
                        --
                        -- , prop_outputsSatisfyMinAdaRequirement tx
                        ]
            Left (ErrBalanceTxAssetsInsufficient err) -> do
                let shortfall = view #shortfall err
                    shortfallOfAda = Value.coin shortfall /= mempty
                    shortfallOfNonAdaAssets = not (Value.isAdaOnly shortfall)
                counterexample (show err) $
                    case (shortfallOfAda, shortfallOfNonAdaAssets) of
                        (False, False) ->
                            -- This case should never occur, as the existence
                            -- of a shortfall implies that we are short of at
                            -- least one asset.
                            property False
                        (True, False) ->
                            label "shortfall of ada"
                                $ property True
                        (False, True) ->
                            label "shortfall of non-ada assets"
                                $ property True
                        (True, True) ->
                            label "shortfall of both ada and non-ada assets"
                                $ property True
            Left (ErrBalanceTxUpdateError (ErrExistingKeyWitnesses _)) ->
                label "existing key wits" $ property True
            Left
                (ErrBalanceTxOutputError
                (ErrBalanceTxOutputErrorOf _index
                (ErrBalanceTxOutputAdaQuantityInsufficient _e))) ->
                label "output below minCoinValue" $ property True
            Left (ErrBalanceTxExistingCollateral) ->
                label "existing collateral" True
            Left (ErrBalanceTxExistingTotalCollateral) ->
                label "existing total collateral" True
            Left (ErrBalanceTxExistingReturnCollateral) ->
                label "existing collateral return outputs" True
            Left ErrBalanceTxMaxSizeLimitExceeded ->
                label "maxTxSize limit exceeded" $ property True
            Left ErrBalanceTxConflictingNetworks ->
                label "conflicting networks" $ property True
            Left ErrBalanceTxUnableToCreateInput ->
                label "unable to create input" $ property True
            Left (ErrBalanceTxInternalError
                 (ErrUnderestimatedFee delta candidateTx nWits)) ->
                let counterexampleText = unlines
                        [ "underestimated fee by "
                            <> pretty (Convert.toWalletCoin delta)
                        , "candidate tx: " <> pretty candidateTx
                        , "assuming key witness count: " <> show nWits
                        ]
                in
                    counterexample counterexampleText $ property False
            Left
                (ErrBalanceTxAssignRedeemers
                (ErrAssignRedeemersTranslationError
                (ByronTxOutInContext _))) ->
                label "failed with ByronTxOutInContext" $ property True
            Left
                (ErrBalanceTxAssignRedeemers
                (ErrAssignRedeemersTranslationError
                (ReferenceScriptsNotSupported _))) ->
                -- Possible with PlutusV1
                label "ReferenceScriptsNotSupported" $ property True
            Left ErrBalanceTxUnableToCreateChange {} ->
                label "unable to create change" $ property True
            Left ErrBalanceTxInputResolutionConflicts{} ->
                label "input resolution conflicts" $ property True
            Left err -> label "other error" $
                counterexample ("balanceTransaction failed: " <> show err) False
  where
    prop_expectFeeExcessSmallerThan
        :: Cardano.Lovelace
        -> Cardano.Tx era
        -> UTxO (ShelleyLedgerEra era)
        -> Property
    prop_expectFeeExcessSmallerThan lim tx utxo = do
        let fee = txFee tx
        let minfee = minFee tx utxo
        let unLovelace (Cardano.Lovelace x) = x
        let delta = Cardano.Lovelace $ (unLovelace fee) - (unLovelace minfee)
        let msg = unwords
                [ "The fee", showInParens fee
                , "was", show delta
                , "larger than the minimum fee", showInParens minfee <> ","
                , "which is a larger difference than", show lim
                ]
        counterexample msg $ property $ delta < lim
      where
        showInParens x = "(" <> show x <> ")"

    prop_minfeeIsCovered
        :: Cardano.Tx era
        -> UTxO (ShelleyLedgerEra era)
        -> Property
    prop_minfeeIsCovered tx utxo = do
        let fee = txFee tx
        let minfee = minFee tx utxo
        let unLovelace (Cardano.Lovelace x) = x
        let delta = Cardano.Lovelace $ (unLovelace minfee) - (unLovelace fee)
        let msg = unwords
                [ "The minimum fee was"
                , show minfee
                , "but the actual fee,"
                , show fee
                , "was lower by"
                , show delta
                ]
        counterexample msg $ property $ fee >= minfee

    prop_validSize
        :: Cardano.Tx era
        -> UTxO (ShelleyLedgerEra era)
        -> Property
    prop_validSize tx@(Cardano.Tx body _) utxo = do
        let era = recentEra @era
        let (TxSize size) =
                estimateSignedTxSize era ledgerPParams
                    (estimateKeyWitnessCount utxo body)
                    (fromCardanoTx tx)
        let limit = ledgerPParams ^. ppMaxTxSizeL
        let msg = unwords
                [ "The tx size "
                , show size
                , "must be lower than the maximum size "
                , show limit
                , ", tx:"
                , show tx
                ]
        counterexample msg $ property (size <= limit)

    _prop_outputsSatisfyMinAdaRequirement
        :: Cardano.Tx era
        -> Property
    _prop_outputsSatisfyMinAdaRequirement (Cardano.ShelleyTx _ tx) = do
        let outputs = Write.outputs era $ Write.txBody era tx
        conjoin $ map valid outputs
      where
        era = recentEra @era

        valid :: TxOut (Cardano.ShelleyLedgerEra era) -> Property
        valid out = counterexample msg $ property $
            not $ Write.isBelowMinimumCoinForTxOut era ledgerPParams out
          where
            msg = unwords
                [ "ada quantity is"
                , "below minimum requirement"
                , "\nin\n"
                , show out
                , "\n"
                , "Suggested ada quantity (may overestimate requirement):"
                , show $ Write.computeMinimumCoinForTxOut
                    (recentEra @era)
                    ledgerPParams
                    out
                ]

    hasZeroAdaOutputs :: Cardano.Tx era -> Bool
    hasZeroAdaOutputs (Cardano.ShelleyTx _ tx) =
        any hasZeroAda (Write.outputs era $ Write.txBody era tx)
      where
        era = recentEra @era
        hasZeroAda (Write.BabbageTxOut _ val _ _) =
            Value.coin val == Ledger.Coin 0

    hasCollateral :: Cardano.Tx era -> Bool
    hasCollateral (Cardano.Tx (Cardano.TxBody content) _) =
        case Cardano.txInsCollateral content of
            Cardano.TxInsCollateralNone -> False
            Cardano.TxInsCollateral _ [] -> False
            Cardano.TxInsCollateral _ (_:_) -> True

    txFee :: Cardano.Tx era -> Cardano.Lovelace
    txFee (Cardano.Tx (Cardano.TxBody content) _) =
        case Cardano.txFee content of
            Cardano.TxFeeExplicit _ c -> c
            Cardano.TxFeeImplicit i -> case i of {}

    minFee
        :: Cardano.Tx era
        -> UTxO (ShelleyLedgerEra era)
        -> Cardano.Lovelace
    minFee tx@(Cardano.Tx body _) utxo = Write.toCardanoLovelace
        $ Write.evaluateMinimumFee (recentEra @era) ledgerPParams
            (fromCardanoTx tx)
            (estimateKeyWitnessCount utxo body)

    txBalance
        :: Cardano.Tx era
        -> UTxO (ShelleyLedgerEra era)
        -> Cardano.Value
    txBalance tx u = Write.toCardanoValue @era $
        Write.evaluateTransactionBalance
            era
            ledgerPParams
            u
            (Write.txBody era $ fromCardanoTx tx)
      where
        era = recentEra @era

    ledgerPParams = Write.pparamsLedger $
        mockPParamsForBalancing @era

    txOutputs :: Cardano.Tx era -> [Cardano.TxOut Cardano.CtxTx era]
    txOutputs (Cardano.Tx (Cardano.TxBody content) _) =
        Cardano.txOuts content

{-# ANN prop_bootstrapWitnesses ("HLint: ignore Eta reduce" :: String) #-}
prop_bootstrapWitnesses
    :: (Word8 -> InAnyRecentEra Cardano.Tx -> Property)
    -> Word8
    -- ^ Number of bootstrap witnesses.
    --
    -- Testing with [0, 255] should be sufficient.
    -> AnyRecentEra
    -> Cardano.NetworkId
    -- ^ Network - will be encoded inside the witness.
    -> Index 'WholeDomain 'AccountK
    -- ^ Account index - will be encoded inside the witness.
    -> Index 'WholeDomain 'CredFromKeyK
    -- ^ Index for the first of the 'n' addresses.
    -> Property
prop_bootstrapWitnesses
    p n (AnyRecentEra (era :: RecentEra era)) net accIx addr0Ix =
    let
        -- Start incrementing the ixs upward, and if we reach 'maxBound', loop
        -- around, to ensure we always have 'n' unique indices.
        addrIxs = take (fromIntegral n)
            $ [addr0Ix .. maxBound] ++ filter (< addr0Ix) [minBound .. addr0Ix]
        body = emptyCardanoTxBody
        wits :: [Cardano.KeyWitness era]
        wits = map (dummyWitForIx body) addrIxs
    in
        p n (InAnyRecentEra era $ Cardano.Tx body wits)
  where
    emptyCardanoTxBody = body
      where
        Cardano.Tx body _ = toCardanoTx $ Write.emptyTx era

    rootK = Byron.generateKeyFromSeed dummyMnemonic mempty
    pwd = mempty

    dummyWitForIx
        :: Cardano.TxBody era
        -> Index 'WholeDomain 'CredFromKeyK
        -> Cardano.KeyWitness era
    dummyWitForIx body ix =
        let
            accK = Byron.deriveAccountPrivateKey pwd rootK accIx
            addrKeyAtIx i = Byron.deriveAddressPrivateKey pwd accK i

            addrK = addrKeyAtIx $ toEnum $ fromEnum ix
            addr = case net of
                Cardano.Mainnet ->
                    paymentAddress SMainnet $ publicKey ByronKeyS addrK
                Cardano.Testnet _magic ->
                    -- The choice of network magic here is not important. The
                    -- size of the witness will not be affected by it. What may
                    -- affect the size, is the 'Cardano.NetworkId' we pass to
                    -- 'mkByronWitness' above.
                    withSNetworkId (NTestnet 0) $ \testnet ->
                        paymentAddress testnet $ publicKey ByronKeyS addrK
        in
            case era of
                RecentEraConway ->
                    mkByronWitness body net addr
                        (getRawKey ByronKeyS addrK, pwd)
                RecentEraBabbage ->
                    mkByronWitness body net addr
                        (getRawKey ByronKeyS addrK, pwd)

-- A helper function to generate properties for 'distributeSurplus' on
-- success.
--
prop_distributeSurplus_onSuccess
    :: Testable prop
    => (FeePerByte
        -> W.Coin
        -> TxFeeAndChange [W.TxOut]
        -> TxFeeAndChange [W.TxOut]
        -> prop)
    -> FeePerByte
    -> TxBalanceSurplus W.Coin
    -> TxFeeAndChange [W.TxOut]
    -> Property
prop_distributeSurplus_onSuccess propertyToTest policy txSurplus fc =
    checkCoverage $
    cover 50
        (isRight mResult)
        "isRight mResult" $
    cover 10
        (length changeOriginal == 0)
        "length changeOriginal == 0" $
    cover 10
        (length changeOriginal == 1)
        "length changeOriginal == 1" $
    cover 50
        (length changeOriginal >= 2)
        "length changeOriginal >= 2" $
    cover 2
        (feeOriginal == W.Coin 0)
        "feeOriginal == W.Coin 0" $
    cover 2
        (feeOriginal == W.Coin 1)
        "feeOriginal == W.Coin 1" $
    cover 50
        (feeOriginal >= W.Coin 2)
        "feeOriginal >= W.Coin 2" $
    cover 1
        (surplus == W.Coin 0)
        "surplus == W.Coin 0" $
    cover 1
        (surplus == W.Coin 1)
        "surplus == W.Coin 1" $
    cover 50
        (surplus >= W.Coin 2)
        "surplus >= W.Coin 2" $
    either
        (const $ property True)
        (property . propertyToTest policy surplus fc)
        mResult
  where
    TxBalanceSurplus surplus = txSurplus
    TxFeeAndChange feeOriginal changeOriginal = fc

    mResult :: Either ErrMoreSurplusNeeded (TxFeeAndChange [W.TxOut])
    mResult = distributeSurplus policy surplus fc

-- Verifies that the 'distributeSurplus' function conserves the surplus: the
-- total increase in the fee and change ada quantities should be exactly equal
-- to the given surplus.
--
prop_distributeSurplus_onSuccess_conservesSurplus
    :: FeePerByte
    -> TxBalanceSurplus W.Coin
    -> TxFeeAndChange [W.TxOut]
    -> Property
prop_distributeSurplus_onSuccess_conservesSurplus =
    prop_distributeSurplus_onSuccess $ \_policy surplus
        (TxFeeAndChange feeOriginal changeOriginal)
        (TxFeeAndChange feeModified changeModified) ->
        surplus ===
            (feeModified <> F.foldMap W.TxOut.coin changeModified)
            <\>
            (feeOriginal <> F.foldMap W.TxOut.coin changeOriginal)

-- The 'distributeSurplus' function should cover the cost of any increases in
-- 'Coin' values.
--
-- If the total cost of encoding ada quantities has increased by 𝛿c, then the
-- fee value should have increased by at least 𝛿c.
--
prop_distributeSurplus_onSuccess_coversCostIncrease
    :: FeePerByte
    -> TxBalanceSurplus W.Coin
    -> TxFeeAndChange [W.TxOut]
    -> Property
prop_distributeSurplus_onSuccess_coversCostIncrease =
    prop_distributeSurplus_onSuccess $ \policy _surplus
        (TxFeeAndChange feeOriginal changeOriginal)
        (TxFeeAndChange feeModified changeModified) -> do
        let coinsOriginal = feeOriginal : (W.TxOut.coin <$> changeOriginal)
        let coinsModified = feeModified : (W.TxOut.coin <$> changeModified)
        let coinDeltas = zipWith (<\>) coinsModified coinsOriginal
        let costIncrease = F.foldMap
                (uncurry $ costOfIncreasingCoin policy)
                (coinsOriginal `zip` coinDeltas)
        feeModified <\> feeOriginal >= costIncrease
            & report feeModified "feeModified"
            & report feeOriginal "feeOriginal"
            & report costIncrease "costIncrease"

-- Since the 'distributeSurplus' function is not aware of the minimum ada
-- quantity or how to calculate it, it should never allow change ada values to
-- decrease.
--
prop_distributeSurplus_onSuccess_doesNotReduceChangeCoinValues
    :: FeePerByte
    -> TxBalanceSurplus W.Coin
    -> TxFeeAndChange [W.TxOut]
    -> Property
prop_distributeSurplus_onSuccess_doesNotReduceChangeCoinValues =
    prop_distributeSurplus_onSuccess $ \_policy _surplus
        (TxFeeAndChange _feeOriginal changeOriginal)
        (TxFeeAndChange _feeModified changeModified) ->
            all (uncurry (<=)) $ zip
                (W.TxOut.coin <$> changeOriginal)
                (W.TxOut.coin <$> changeModified)

-- The 'distributeSurplus' function should never return a 'fee' value that is
-- less than the original value.
--
prop_distributeSurplus_onSuccess_doesNotReduceFeeValue
    :: FeePerByte
    -> TxBalanceSurplus W.Coin
    -> TxFeeAndChange [W.TxOut]
    -> Property
prop_distributeSurplus_onSuccess_doesNotReduceFeeValue =
    prop_distributeSurplus_onSuccess $ \_policy _surplus
        (TxFeeAndChange feeOriginal _changeOriginal)
        (TxFeeAndChange feeModified _changeModified) ->
            feeOriginal <= feeModified

-- The 'distributeSurplus' function should increase values by the exact amounts
-- indicated in 'distributeSurplusDelta'.
--
-- This is actually an implementation detail of 'distributeSurplus'.
--
-- However, it's useful to verify that this is true by subtracting the delta
-- values from the result of 'distributeSurplus', which should produce the
-- original fee and change values.
--
prop_distributeSurplus_onSuccess_increasesValuesByDelta
    :: FeePerByte
    -> TxBalanceSurplus W.Coin
    -> TxFeeAndChange [W.TxOut]
    -> Property
prop_distributeSurplus_onSuccess_increasesValuesByDelta =
    prop_distributeSurplus_onSuccess $ \policy surplus
        (TxFeeAndChange feeOriginal changeOriginal)
        (TxFeeAndChange feeModified changeModified) ->
            let (TxFeeAndChange feeDelta changeDeltas) =
                    either (error . show) id
                    $ distributeSurplusDelta policy surplus
                    $ TxFeeAndChange
                        (feeOriginal)
                        (W.TxOut.coin <$> changeOriginal)
            in
            (TxFeeAndChange
                (feeModified <\> feeDelta)
                (zipWith W.TxOut.subtractCoin changeDeltas changeModified)
            )
            ===
            TxFeeAndChange feeOriginal changeOriginal

-- The 'distributeSurplus' function should only adjust the very first change
-- value.  All other change values should be left untouched.
--
-- This is actually an implementation detail of 'distributeSurplus'.
--
-- In principle, 'distributeSurplus' could allow itself to adjust any of the
-- change values in order to find a (marginally) more optimal solution.
-- However, for reasons of simplicity, we only adjust the first change value.
--
-- Here we verify that the implementation indeed only adjusts the first change
-- value, as expected.
--
prop_distributeSurplus_onSuccess_onlyAdjustsFirstChangeValue
    :: FeePerByte
    -> TxBalanceSurplus W.Coin
    -> TxFeeAndChange [W.TxOut]
    -> Property
prop_distributeSurplus_onSuccess_onlyAdjustsFirstChangeValue =
    prop_distributeSurplus_onSuccess $ \_policy _surplus
        (TxFeeAndChange _feeOriginal changeOriginal)
        (TxFeeAndChange _feeModified changeModified) ->
            (drop 1 changeOriginal) ===
            (drop 1 changeModified)

-- The 'distributeSurplus' function should never adjust addresses of change
-- outputs.
--
prop_distributeSurplus_onSuccess_preservesChangeAddresses
    :: FeePerByte
    -> TxBalanceSurplus W.Coin
    -> TxFeeAndChange [W.TxOut]
    -> Property
prop_distributeSurplus_onSuccess_preservesChangeAddresses =
    prop_distributeSurplus_onSuccess $ \_policy _surplus
        (TxFeeAndChange _feeOriginal changeOriginal)
        (TxFeeAndChange _feeModified changeModified) ->
            (view #address <$> changeOriginal) ===
            (view #address <$> changeModified)

-- The 'distributeSurplus' function should always return exactly the same
-- number of change outputs that it was given. It should never create or
-- destroy change outputs.
--
prop_distributeSurplus_onSuccess_preservesChangeLength
    :: FeePerByte
    -> TxBalanceSurplus W.Coin
    -> TxFeeAndChange [W.TxOut]
    -> Property
prop_distributeSurplus_onSuccess_preservesChangeLength =
    prop_distributeSurplus_onSuccess $ \_policy _surplus
        (TxFeeAndChange _feeOriginal changeOriginal)
        (TxFeeAndChange _feeModified changeModified) ->
            length changeOriginal === length changeModified

-- The 'distributeSurplus' function should never adjust the values of non-ada
-- assets.
--
prop_distributeSurplus_onSuccess_preservesChangeNonAdaAssets
    :: FeePerByte
    -> TxBalanceSurplus W.Coin
    -> TxFeeAndChange [W.TxOut]
    -> Property
prop_distributeSurplus_onSuccess_preservesChangeNonAdaAssets =
    prop_distributeSurplus_onSuccess $ \_policy _surplus
        (TxFeeAndChange _feeOriginal changeOriginal)
        (TxFeeAndChange _feeModified changeModified) ->
            (view (#tokens . #tokens) <$> changeOriginal) ===
            (view (#tokens . #tokens) <$> changeModified)

-- Verify that 'distributeSurplusDelta':
--
--    - covers the increase to the fee requirement incurred as a result of
--      increasing the fee value and change values.
--
--    - conserves the surplus:
--        - feeDelta + sum changeDeltas == surplus
--
prop_distributeSurplusDelta_coversCostIncreaseAndConservesSurplus
    :: FeePerByte -> W.Coin -> W.Coin -> [W.Coin] -> Property
prop_distributeSurplusDelta_coversCostIncreaseAndConservesSurplus
    feePolicy surplus fee0 change0 =
    checkCoverage $
    cover 2  (isLeft  mres) "Failure" $
    cover 50 (isRight mres) "Success" $
    report mres "Result" $
    counterexample (show mres) $ case mres of
        Left (ErrMoreSurplusNeeded shortfall) ->
            conjoin
                [ property $ surplus < maxCoinCostIncrease
                , property $ shortfall > W.Coin 0
                , costOfIncreasingCoin feePolicy fee0 surplus
                    === surplus <> shortfall
                ]
        Right (TxFeeAndChange feeDelta changeDeltas) -> do
            let feeRequirementIncrease = mconcat
                    [ costOfIncreasingCoin feePolicy fee0 feeDelta
                    , F.fold $ zipWith (costOfIncreasingCoin feePolicy)
                        change0
                        changeDeltas
                    ]
            conjoin
                [ property $ feeDelta >= feeRequirementIncrease
                    & counterexample ("fee requirement increased by "
                        <> show feeRequirementIncrease
                        <> " but the fee delta was just "
                        <> show feeDelta
                        )
                , F.fold changeDeltas <> feeDelta
                    === surplus
                ]
  where
    mres = distributeSurplusDelta
        feePolicy surplus (TxFeeAndChange fee0 change0)
    maxCoinCostIncrease = maximumCostOfIncreasingCoin feePolicy

prop_posAndNegFromCardanoValueRoundtrip :: Property
prop_posAndNegFromCardanoValueRoundtrip = forAll genSignedValue $ \v ->
    let
        (pos, neg) = posAndNegFromCardanoValue v
    in
        walletToCardanoValue pos <>
        (Cardano.negateValue (walletToCardanoValue neg))
        === v

prop_updateTx
    :: InAnyRecentEra Cardano.Tx
    -> [(W.TxIn, W.TxOut)]
    -> [W.TxIn]
    -> [W.TxOut]
    -> W.Coin
    -> Property
prop_updateTx
    (InAnyRecentEra _era tx)
    extraIns extraCol extraOuts newFee =
    do
        let extra = TxUpdate extraIns extraCol extraOuts [] (UseNewTxFee newFee)
        let tx' = either (error . show) id
                $ updateTx tx extra
        conjoin
            [ inputs tx' === inputs tx <> Set.fromList (fst <$> extraIns)
            , outputs tx' === outputs tx <> Set.fromList extraOuts
            , sealedFee tx' === Just newFee
            , collateralIns tx' === collateralIns tx <> Set.fromList extraCol
            ]
  where
    inputs = sealedInputs . sealedTxFromCardano'
    outputs = sealedOutputs . sealedTxFromCardano'
    collateralIns = sealedCollateralInputs . sealedTxFromCardano'

--------------------------------------------------------------------------------
-- Utility types
--------------------------------------------------------------------------------

-- | Encapsulates both a 'ChangeAddressGen s' and the 's' required for the
-- generator. This allows properties like 'prop_balanceTransactionValid' to
-- easily generate arbitrary change address generators.
data AnyChangeAddressGenWithState where
    AnyChangeAddressGenWithState
        :: forall s. ChangeAddressGen s
        -> s
        -> AnyChangeAddressGenWithState

data BalanceTxGolden =
    BalanceTxGoldenSuccess
        W.Coin -- ^ Wallet balance
        Cardano.Lovelace -- ^ Fee
        Cardano.Lovelace -- ^ Minimum fee
    | BalanceTxGoldenFailure W.Coin String
    deriving (Eq, Show)

newtype DummyChangeState = DummyChangeState { nextUnusedIndex :: Int }
    deriving (Show, Eq)

newtype TxBalanceSurplus a = TxBalanceSurplus {unTxBalanceSurplus :: a}
    deriving (Eq, Show)

data Wallet' = Wallet' UTxOAssumptions W.UTxO AnyChangeAddressGenWithState
    deriving Show via (ShowBuildable Wallet')

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

-- Ideally merge with 'updateTx'
addExtraTxIns
    :: [W.TxIn]
    -> PartialTx Cardano.BabbageEra
    -> PartialTx Cardano.BabbageEra
addExtraTxIns extraIns =
    #tx %~ modifyBabbageTxBody (inputsTxBodyL %~ (<> toLedgerInputs extraIns))
  where
    toLedgerInputs =
        Set.map Convert.toLedger . Set.fromList

-- | Wrapper for testing convenience. Does hide the monad 'm', tracing, and the
-- updated 'changeState'. Does /not/ specify mock values for things like
-- protocol parameters. This is up to the caller to provide.
balanceTx
    :: IsRecentEra era
    => Wallet'
    -> Write.ProtocolParameters era
    -> TimeTranslation
    -> StdGenSeed
    -> PartialTx era
    -> Either (ErrBalanceTx era) (Cardano.Tx era)
balanceTx
    (Wallet' utxoAssumptions utxo (AnyChangeAddressGenWithState genChange s))
    protocolParameters
    timeTranslation
    seed
    partialTx
    = (`evalRand` stdGenFromSeed seed) $ runExceptT $ do
        (transactionInEra, _nextChangeState) <-
            balanceTransaction
                utxoAssumptions
                protocolParameters
                timeTranslation
                (constructUTxOIndex utxo)
                genChange
                s
                partialTx
        pure transactionInEra

-- | Also returns the updated change state
balanceTransactionWithDummyChangeState
    :: forall era. IsRecentEra era
    => UTxOAssumptions
    -> W.UTxO
    -> StdGenSeed
    -> PartialTx era
    -> Either (ErrBalanceTx era) (Cardano.Tx era, DummyChangeState)
balanceTransactionWithDummyChangeState utxoAssumptions utxo seed partialTx =
    (`evalRand` stdGenFromSeed seed) $ runExceptT $
        balanceTransaction
            utxoAssumptions
            mockPParamsForBalancing
            dummyTimeTranslation
            (constructUTxOIndex utxo)
            dummyChangeAddrGen
            (getState $ unsafeInitWallet utxo (W.Block.header block0)
                DummyChangeState { nextUnusedIndex = 0 })
            partialTx

cardanoTx :: SealedTx -> InAnyCardanoEra Cardano.Tx
cardanoTx = cardanoTxIdeallyNoLaterThan maxBound

deserializeBabbageTx :: ByteString -> Cardano.Tx Cardano.BabbageEra
deserializeBabbageTx = either (error . show) id
    . Cardano.deserialiseFromCBOR (Cardano.AsTx Cardano.AsBabbageEra)

fst6 :: (a, b, c, d, e, f) -> a
fst6 (a, _, _, _, _, _) = a

hasInsCollateral :: Cardano.Tx era -> Bool
hasInsCollateral (Cardano.Tx (Cardano.TxBody content) _) =
    case Cardano.txInsCollateral content of
        Cardano.TxInsCollateralNone -> False
        Cardano.TxInsCollateral _ [] -> False
        Cardano.TxInsCollateral _ _ -> True

hasReturnCollateral :: Cardano.Tx era -> Bool
hasReturnCollateral (Cardano.Tx (Cardano.TxBody content) _) =
    case Cardano.txReturnCollateral content of
        Cardano.TxReturnCollateralNone -> False
        Cardano.TxReturnCollateral _ _ -> True

hasTotalCollateral :: Cardano.Tx era -> Bool
hasTotalCollateral (Cardano.Tx (Cardano.TxBody content) _) =
    case Cardano.txTotalCollateral content of
        Cardano.TxTotalCollateralNone -> False
        Cardano.TxTotalCollateral _ _ -> True

mkTestWallet :: W.UTxO -> Wallet'
mkTestWallet utxo =
    Wallet' AllKeyPaymentCredentials utxo dummyShelleyChangeAddressGen

mockPParamsForBalancing
    :: forall era . IsRecentEra era => Write.ProtocolParameters era
mockPParamsForBalancing =
    Write.ProtocolParameters . either (error . show) id $
        Cardano.toLedgerPParams
            (Write.shelleyBasedEra @era)
            mockCardanoApiPParamsForBalancing

-- Ideally merge with 'updateTx'
modifyBabbageTxBody
    :: ( Babbage.BabbageTxBody StandardBabbage ->
         Babbage.BabbageTxBody StandardBabbage
       )
    -> Cardano.Tx Cardano.BabbageEra -> Cardano.Tx Cardano.BabbageEra
modifyBabbageTxBody
    f
    (Cardano.Tx
        (Cardano.ShelleyTxBody
            era
            body
            scripts
            scriptData
            auxData
            scriptValidity)
        keyWits)
    = Cardano.Tx
        (Cardano.ShelleyTxBody
            era
            (f body)
            scripts
            scriptData
            auxData
            scriptValidity)
        keyWits

paymentPartialTx :: [W.TxOut] -> PartialTx Cardano.BabbageEra
paymentPartialTx txouts = PartialTx (Cardano.Tx body []) mempty []
  where
    body = Cardano.ShelleyTxBody
        Cardano.ShelleyBasedEraBabbage
        ( mkBasicTxBody &
            outputsTxBodyL .~
            StrictSeq.fromList (Convert.toBabbageTxOut <$> txouts)
        )
        []
        Cardano.TxBodyNoScriptData
        Nothing
        Cardano.TxScriptValidityNone

recentEraTxFromBytes :: ByteString -> InAnyRecentEra Cardano.Tx
recentEraTxFromBytes bytes =
    let
        anyEraTx
            = cardanoTx
            $ either (error . show) id
            $ sealedTxFromBytes bytes
    in
        case Write.asAnyRecentEra anyEraTx of
            Just recentEraTx -> recentEraTx
            Nothing -> error "recentEraTxFromBytes: older eras not supported"

-- | Restricts the inputs list of the 'PartialTx' to the inputs of the
-- underlying CBOR transaction. This allows us to "fix" the 'PartialTx' after
-- shrinking the CBOR.
--
-- NOTE: Perhaps ideally 'PartialTx' would handle this automatically.
restrictResolution :: PartialTx era -> PartialTx era
restrictResolution (PartialTx tx (Cardano.UTxO u) redeemers) =
    let
        u' = u `Map.restrictKeys` (inputsInTx tx)
    in
        PartialTx tx (Cardano.UTxO u') redeemers
  where
    inputsInTx (Cardano.Tx (Cardano.TxBody bod) _) =
        Set.fromList $ map fst $ Cardano.txIns bod

sealedCollateralInputs :: SealedTx -> Set W.TxIn
sealedCollateralInputs =
    Set.fromList
    . map fst
    . view #resolvedCollateralInputs
    . fst6
    . _decodeSealedTx maxBound (ShelleyWalletCtx dummyPolicyK)

sealedFee
    :: forall era. Cardano.IsCardanoEra era
    => Cardano.Tx era
    -> Maybe W.Coin
sealedFee =
    view #fee
    . fst6
    . _decodeSealedTx maxBound (ShelleyWalletCtx dummyPolicyK)
    . sealedTxFromCardano'

sealedInputs :: SealedTx -> Set W.TxIn
sealedInputs =
    Set.fromList
    . map fst
    . view #resolvedInputs
    . fst6
    . _decodeSealedTx maxBound (ShelleyWalletCtx dummyPolicyK)

sealedOutputs :: SealedTx -> Set W.TxOut
sealedOutputs =
    Set.fromList
    . view #outputs
    . fst6
    . _decodeSealedTx maxBound (ShelleyWalletCtx dummyPolicyK)

serializedSize :: forall era. Cardano.IsCardanoEra era => Cardano.Tx era -> Int
serializedSize = BS.length
    . serialisedTx
    . sealedTxFromCardano
    . Cardano.InAnyCardanoEra (Cardano.cardanoEra @era)

-- | Checks for membership in the given closed interval [a, b]
shouldBeInclusivelyWithin :: (Ord a, Show a) => a -> (a, a) -> IO ()
x `shouldBeInclusivelyWithin` (a, b) =
    if a <= x && x <= b
    then pure ()
    else expectationFailure $ unwords
        [ show x
        , "not in the expected interval"
        , "[" <> show a <> ", " <> show b <> "]"
        ]

txMinFee
    :: Cardano.Tx Cardano.BabbageEra
    -> Cardano.UTxO Cardano.BabbageEra
    -> Cardano.Lovelace
txMinFee tx@(Cardano.Tx body _) u =
    Write.toCardanoLovelace $
        Write.evaluateMinimumFee
            RecentEraBabbage
            (Write.pparamsLedger $ mockPParamsForBalancing @Cardano.BabbageEra)
            (fromCardanoTx tx)
            (estimateKeyWitnessCount (fromCardanoUTxO u) body)

unsafeSealedTxFromHex :: ByteString -> IO SealedTx
unsafeSealedTxFromHex =
    either (fail . show) pure
        . sealedTxFromBytes
        . unsafeFromHex
        . BS.dropWhileEnd isNewlineChar
  where
    isNewlineChar c = c `elem` [10,13]

withValidityInterval
    :: ValidityInterval
    -> PartialTx Cardano.BabbageEra
    -> PartialTx Cardano.BabbageEra
withValidityInterval vi = #tx %~ modifyBabbageTxBody (vldtTxBodyL .~ vi)

walletToCardanoValue :: W.TokenBundle -> Cardano.Value
walletToCardanoValue = Cardano.fromMaryValue . Convert.toLedger

cardanoToWalletValue :: Cardano.Value -> W.TokenBundle
cardanoToWalletValue = Convert.toWallet . Cardano.toMaryValue

cardanoToWalletCoin :: Cardano.Lovelace -> W.Coin
cardanoToWalletCoin = Convert.toWallet . Cardano.toShelleyLovelace

cardanoToWalletTxOut
    :: forall era. IsRecentEra era
    => Cardano.TxOut Cardano.CtxUTxO era
    -> W.TxOut
cardanoToWalletTxOut =
    toWallet . Cardano.toShelleyTxOut (Write.shelleyBasedEra @era)
  where
    toWallet :: TxOut (ShelleyLedgerEra era) -> W.TxOut
    toWallet x = case recentEra @era of
        RecentEraBabbage -> Convert.fromBabbageTxOut x
        RecentEraConway -> Convert.fromConwayTxOut x

--------------------------------------------------------------------------------
-- Test values
--------------------------------------------------------------------------------

block0 :: W.Block
block0 = W.Block
    { header = W.BlockHeader
        { slotNo = SlotNo 0
        , blockHeight = W.Quantity 0
        , headerHash = W.mockHash $ SlotNo 0
        , parentHeaderHash = Nothing
        }
    , transactions = []
    , delegations = []
    }

{- HLINT ignore costModelsForTesting "Use underscore" -}
costModelsForTesting :: Alonzo.CostModels
costModelsForTesting = either (error . show) id $ do
    v1 <- Alonzo.mkCostModel PlutusV1
        [ 197209, 0, 1, 1, 396231, 621, 0, 1, 150000, 1000, 0, 1, 150000
        , 32, 2477736, 29175, 4, 29773, 100, 29773, 100, 29773, 100
        , 29773, 100, 29773, 100, 29773, 100, 100, 100, 29773, 100
        , 150000, 32, 150000, 32, 150000, 32, 150000, 1000, 0, 1
        , 150000, 32, 150000, 1000, 0, 8, 148000, 425507, 118, 0, 1, 1
        , 150000, 1000, 0, 8, 150000, 112536, 247, 1, 150000, 10000, 1
        , 136542, 1326, 1, 1000, 150000, 1000, 1, 150000, 32, 150000
        , 32, 150000, 32, 1, 1, 150000, 1, 150000, 4, 103599, 248, 1
        , 103599, 248, 1, 145276, 1366, 1, 179690, 497, 1, 150000, 32
        , 150000, 32, 150000, 32, 150000, 32, 150000, 32, 150000, 32
        , 148000, 425507, 118, 0, 1, 1, 61516, 11218, 0, 1, 150000, 32
        , 148000, 425507, 118, 0, 1, 1, 148000, 425507, 118, 0, 1, 1
        , 2477736, 29175, 4, 0, 82363, 4, 150000, 5000, 0, 1, 150000
        , 32, 197209, 0, 1, 1, 150000, 32, 150000, 32, 150000, 32, 150000
        , 32, 150000, 32, 150000, 32, 150000, 32, 3345831, 1, 1
        ]
    v2 <- Alonzo.mkCostModel PlutusV2
        [ 205665, 812, 1, 1, 1000, 571, 0, 1, 1000, 24177, 4, 1, 1000
        , 32, 117366, 10475, 4, 23000, 100, 23000, 100, 23000, 100, 23000
        , 100, 23000, 100, 23000, 100, 100, 100, 23000, 100, 19537, 32
        , 175354, 32, 46417, 4, 221973, 511, 0, 1, 89141, 32, 497525, 14068
        , 4, 2, 196500, 453240, 220, 0, 1, 1, 1000, 28662, 4, 2, 245000
        , 216773, 62, 1, 1060367, 12586, 1, 208512, 421, 1, 187000, 1000
        , 52998, 1, 80436, 32, 43249, 32, 1000, 32, 80556, 1, 57667, 4, 1000
        , 10, 197145, 156, 1, 197145, 156, 1, 204924, 473, 1, 208896, 511, 1
        , 52467, 32, 64832, 32, 65493, 32, 22558, 32, 16563, 32, 76511, 32
        , 196500, 453240, 220, 0, 1, 1, 69522, 11687, 0, 1, 60091, 32, 196500
        , 453240, 220, 0, 1, 1, 196500, 453240, 220, 0, 1, 1, 1159724, 392670
        , 0, 2, 806990, 30482, 4, 1927926, 82523, 4, 265318, 0, 4, 0, 85931
        , 32, 205665, 812, 1, 1, 41182, 32, 212342, 32, 31220, 32, 32696, 32
        , 43357, 32, 32247, 32, 38314, 32, 20000000000, 20000000000, 9462713
        , 1021, 10, 20000000000, 0, 20000000000
        ]
    pure Alonzo.CostModels
        { costModelsValid = Map.fromList [(PlutusV1, v1), (PlutusV2, v2)]
        , costModelsErrors = Map.empty
        , costModelsUnknown = Map.empty
        }

dummyChangeAddrGen :: ChangeAddressGen DummyChangeState
dummyChangeAddrGen = ChangeAddressGen
    { getChangeAddressGen = \(DummyChangeState i) ->
        (addressAtIx $ toEnum i, DummyChangeState $ succ i)
    , maxLengthChangeAddress = addressAtIx minBound
    }
  where
    addressAtIx
        :: Index
            'Cardano.Wallet.Address.Derivation.Soft
            'CredFromKeyK
        -> Write.Address
    addressAtIx ix = Convert.toLedgerAddress
        $ paymentAddress @ShelleyKey @'CredFromKeyK SMainnet
        $ publicKey ShelleyKeyS
        $ Shelley.ShelleyKey
        $ Shelley.deriveAddressPrivateKeyShelley
            pwd
            acctK
            Cardano.Wallet.Address.Derivation.UtxoInternal
            ix
    pwd = Passphrase ""
    rootK = Shelley.unsafeGenerateKeyFromSeed (dummyMnemonic, Nothing) pwd
    acctK = Shelley.deriveAccountPrivateKeyShelley
        purposeBIP44
        pwd
        (getRawKey ShelleyKeyS rootK)
        minBound

dummyMnemonic :: SomeMnemonic
dummyMnemonic = SomeMnemonic $ either (error . show) id
    (entropyToMnemonic @12 <$> mkEntropy "0000000000000000")

dummyPolicyK :: KeyHash
dummyPolicyK = KeyHash Policy (BS.replicate 32 0)

-- Byron style addresses, corresponding to the change addresses generated by
-- "byron wallets".
dummyByronChangeAddressGen :: AnyChangeAddressGenWithState
dummyByronChangeAddressGen = AnyChangeAddressGenWithState
    (defaultChangeAddressGen @(RndState 'Mainnet) (byronRootK, pwd))
    (mkRndState byronRootK 0)
  where
    byronRootK = Byron.generateKeyFromSeed mw mempty
    mw = SomeMnemonic $ either (error . show) id
        (entropyToMnemonic @12 <$> mkEntropy "0000000000000000")
    pwd = mempty

-- | Shelley base addresses, corresponding to the change addresses generated by
-- normal shelley wallets.
dummyShelleyChangeAddressGen :: AnyChangeAddressGenWithState
dummyShelleyChangeAddressGen = AnyChangeAddressGenWithState
    (defaultChangeAddressGen @(SeqState 'Mainnet ShelleyKey)
        (delegationAddress @ShelleyKey SMainnet)
        )
    (mkSeqStateFromRootXPrv ShelleyKeyS
        (RootCredentials rootK pwd)
        purposeCIP1852
        defaultAddressPoolGap)
  where
    pwd = Passphrase ""
    rootK = Shelley.unsafeGenerateKeyFromSeed (dummyMnemonic, Nothing) mempty

dummyTimeTranslation :: TimeTranslation
dummyTimeTranslation =
    timeTranslationFromEpochInfo
        (Slotting.SystemStart (posixSecondsToUTCTime 0))
        (Slotting.fixedEpochInfo
            (Slotting.EpochSize 21_600)
            (Slotting.slotLengthFromSec 1))

-- | A dummy 'TimeTranslation' for testing 'PastHorizonException's.
dummyTimeTranslationWithHorizon :: SlotNo -> TimeTranslation
dummyTimeTranslationWithHorizon horizon =
    timeTranslationFromEpochInfo systemStart epochInfo
  where
    slotLength = 1
    t0 = HF.initBound
    t1 = HF.Bound
        (RelativeTime $ fromIntegral $ slotLength * (unSlotNo horizon))
        horizon
        (Cardano.EpochNo 1)

    era1Params = HF.defaultEraParams (SecurityParam 2) (mkSlotLength 1)
    summary = HF.summaryWithExactly
        (exactlyOne (HF.EraSummary t0 (HF.EraEnd t1) era1Params))

    systemStart :: Slotting.SystemStart
    systemStart = Slotting.SystemStart (posixSecondsToUTCTime 0)

    epochInfo :: Slotting.EpochInfo (Either PastHorizonException)
    epochInfo = Slotting.hoistEpochInfo runExcept
        (HF.interpreterToEpochInfo (HF.mkInterpreter summary))

mainnetFeePerByte :: FeePerByte
mainnetFeePerByte = FeePerByte 44

-- | We try to use similar parameters to mainnet where it matters (in particular
-- fees, execution unit prices, and the cost model.)
mockCardanoApiPParamsForBalancing
    :: Cardano.ProtocolParameters
mockCardanoApiPParamsForBalancing = Cardano.ProtocolParameters
    { Cardano.protocolParamTxFeeFixed = 155_381
    , Cardano.protocolParamTxFeePerByte = 44
    , Cardano.protocolParamMaxTxSize = 16_384
    , Cardano.protocolParamMinUTxOValue = Nothing
    , Cardano.protocolParamMaxTxExUnits =
        Just $ Cardano.ExecutionUnits 10_000_000_000 14_000_000
    , Cardano.protocolParamMaxValueSize = Just 4_000
    , Cardano.protocolParamProtocolVersion = (6, 0)
    , Cardano.protocolParamDecentralization = Just 0
    , Cardano.protocolParamExtraPraosEntropy = Nothing
    , Cardano.protocolParamMaxBlockHeaderSize = 100_000 -- Dummy value
    , Cardano.protocolParamMaxBlockBodySize = 100_000
    , Cardano.protocolParamStakeAddressDeposit = Cardano.Lovelace 2_000_000
    , Cardano.protocolParamStakePoolDeposit = Cardano.Lovelace 500_000_000
    , Cardano.protocolParamMinPoolCost = Cardano.Lovelace 32_000_000
    , Cardano.protocolParamPoolRetireMaxEpoch = Cardano.EpochNo 2
    , Cardano.protocolParamStakePoolTargetNum = 100
    , Cardano.protocolParamPoolPledgeInfluence = 0
    , Cardano.protocolParamMonetaryExpansion = 0
    , Cardano.protocolParamTreasuryCut  = 0
    , Cardano.protocolParamUTxOCostPerWord =
        Just $ fromShelleyLovelace $
            Alonzo.unCoinPerWord testParameter_coinsPerUTxOWord_Alonzo
    , Cardano.protocolParamUTxOCostPerByte =
        Just $ fromShelleyLovelace $
            Babbage.unCoinPerByte testParameter_coinsPerUTxOByte_Babbage
    , Cardano.protocolParamCostModels =
        Cardano.fromAlonzoCostModels costModelsForTesting
    , Cardano.protocolParamPrices =
        Just $ Cardano.ExecutionUnitPrices (721 % 10_000_000) (577 % 10_000)
    , Cardano.protocolParamMaxBlockExUnits =
        Just $ Cardano.ExecutionUnits 10_000_000_000 14_000_000
    , Cardano.protocolParamCollateralPercent = Just 150
    , Cardano.protocolParamMaxCollateralInputs = Just 3
    }

pingPong_1 :: PartialTx Cardano.BabbageEra
pingPong_1 = PartialTx tx mempty []
  where
    tx = deserializeBabbageTx $ unsafeFromHex $ mconcat
        [ "84a500800d80018183581d714d72cf569a339a18a7d9302313983f56e0d96cd4"
        , "5bdcb1d6512dca6a1a001e84805820923918e403bf43c34b4ef6b48eb2ee04ba"
        , "bed17320d8d1b9ff9ad086e86f44ec02000e80a10481d87980f5f6"
        ]

pingPong_2 :: PartialTx Cardano.BabbageEra
pingPong_2 = PartialTx
    { tx = deserializeBabbageTx $ mconcat
        [ unsafeFromHex "84a50081825820"
        , tid
        , unsafeFromHex "000d80018183581d714d72cf569a339a18a7d9302313983f56e0d96cd45bdcb1d6512dca6a1a001e848058208392f0c940435c06888f9bdb8c74a95dc69f156367d6a089cf008ae05caae01e02000e80a20381591b72591b6f01000033233332222333322223322332232323332223233322232333333332222222232333222323333222232323322323332223233322232323322332232323333322222332233223322332233223322223223223232533530333330083333573466e1d40192004204f23333573466e1d401d2002205123333573466e1d40212000205323504b35304c3357389201035054310004d49926499263333573466e1d40112004205323333573466e1d40152002205523333573466e1d40192000205723504b35304c3357389201035054310004d49926499263333573466e1cd55cea8012400046601664646464646464646464646666ae68cdc39aab9d500a480008cccccccccc064cd409c8c8c8cccd5cd19b8735573aa004900011980f981d1aba15002302c357426ae8940088d4164d4c168cd5ce2481035054310005b49926135573ca00226ea8004d5d0a80519a8138141aba150093335502e75ca05a6ae854020ccd540b9d728169aba1500733502704335742a00c66a04e66aa0a8098eb4d5d0a8029919191999ab9a3370e6aae754009200023350213232323333573466e1cd55cea80124000466a05266a084eb4d5d0a80118239aba135744a00446a0ba6a60bc66ae712401035054310005f49926135573ca00226ea8004d5d0a8011919191999ab9a3370e6aae7540092000233502733504275a6ae854008c11cd5d09aba2500223505d35305e3357389201035054310005f49926135573ca00226ea8004d5d09aba2500223505935305a3357389201035054310005b49926135573ca00226ea8004d5d0a80219a813bae35742a00666a04e66aa0a8eb88004d5d0a801181c9aba135744a00446a0aa6a60ac66ae71241035054310005749926135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135573ca00226ea8004d5d0a8011919191999ab9a3370ea00290031180f181d9aba135573ca00646666ae68cdc3a801240084603a608a6ae84d55cf280211999ab9a3370ea00690011180e98181aba135573ca00a46666ae68cdc3a80224000460406eb8d5d09aab9e50062350503530513357389201035054310005249926499264984d55cea80089baa001357426ae8940088d4124d4c128cd5ce249035054310004b49926104a1350483530493357389201035054350004a4984d55cf280089baa001135573a6ea80044d55ce9baa0012212330010030022001222222222212333333333300100b00a00900800700600500400300220012212330010030022001122123300100300212001122123300100300212001122123300100300212001212222300400521222230030052122223002005212222300100520011232230023758002640026aa078446666aae7c004940388cd4034c010d5d080118019aba200203323232323333573466e1cd55cea801a4000466600e6464646666ae68cdc39aab9d5002480008cc034c0c4d5d0a80119a8098169aba135744a00446a06c6a606e66ae71241035054310003849926135573ca00226ea8004d5d0a801999aa805bae500a35742a00466a01eeb8d5d09aba25002235032353033335738921035054310003449926135744a00226aae7940044dd50009110919980080200180110009109198008018011000899aa800bae75a224464460046eac004c8004d540d888c8cccd55cf80112804919a80419aa81898031aab9d5002300535573ca00460086ae8800c0b84d5d08008891001091091198008020018900089119191999ab9a3370ea002900011a80418029aba135573ca00646666ae68cdc3a801240044a01046a0526a605466ae712401035054310002b499264984d55cea80089baa001121223002003112200112001232323333573466e1cd55cea8012400046600c600e6ae854008dd69aba135744a00446a0466a604866ae71241035054310002549926135573ca00226ea80048848cc00400c00880048c8cccd5cd19b8735573aa002900011bae357426aae7940088d407cd4c080cd5ce24810350543100021499261375400224464646666ae68cdc3a800a40084a00e46666ae68cdc3a8012400446a014600c6ae84d55cf280211999ab9a3370ea00690001280511a8111a981199ab9c490103505431000244992649926135573aa00226ea8004484888c00c0104488800844888004480048c8cccd5cd19b8750014800880188cccd5cd19b8750024800080188d4068d4c06ccd5ce249035054310001c499264984d55ce9baa0011220021220012001232323232323333573466e1d4005200c200b23333573466e1d4009200a200d23333573466e1d400d200823300b375c6ae854014dd69aba135744a00a46666ae68cdc3a8022400c46601a6eb8d5d0a8039bae357426ae89401c8cccd5cd19b875005480108cc048c050d5d0a8049bae357426ae8940248cccd5cd19b875006480088c050c054d5d09aab9e500b23333573466e1d401d2000230133016357426aae7940308d407cd4c080cd5ce2481035054310002149926499264992649926135573aa00826aae79400c4d55cf280109aab9e500113754002424444444600e01044244444446600c012010424444444600a010244444440082444444400644244444446600401201044244444446600201201040024646464646666ae68cdc3a800a400446660106eb4d5d0a8021bad35742a0066eb4d5d09aba2500323333573466e1d400920002300a300b357426aae7940188d4040d4c044cd5ce2490350543100012499264984d55cea80189aba25001135573ca00226ea80048488c00800c888488ccc00401401000c80048c8c8cccd5cd19b875001480088c018dd71aba135573ca00646666ae68cdc3a80124000460106eb8d5d09aab9e500423500a35300b3357389201035054310000c499264984d55cea80089baa001212230020032122300100320011122232323333573466e1cd55cea80124000466aa016600c6ae854008c014d5d09aba25002235007353008335738921035054310000949926135573ca00226ea8004498480048004448848cc00400c008448004488800c488800848880048004488800c488800848880048004448c8c00400488cc00cc008008004c8c8cc88cc88c8ccc888c8c8c8c8c8ccc888ccc888ccc888c8cccc8888c8cc88c8cccc8888c8cc88c8cc88c8ccc888c8c8cc88c8c8cc88c8c8c8cccc8888c8c8c8c8c8cc88c8cc88cc88ccccccccccccc8888888888888c8c8c8c8c8cccccccc88888888cc88cc88cc88cc88c8ccccc88888c8cc88cc88cc88c8cc88cc88cc88c8cc88c8c8c8cccc8888cccc8888c8888d4d540400108888c8c8c94cd4c24004ccc0140280240205400454cd4c24004cd5ce249025331000910115001109101153353508101003215335309001333573466e1cccc109400cd4c07800488004c0580212002092010910115002153353090013357389201025332000910115002109101150011533535080013300533501b00833303e03f5001323355306012001235355096010012233550990100233553063120012353550990100122335509c0100233704900080080080099a809801180a003003909a9aa84a8080091911a9a80f00091299a984a0098050010a99a984a00999aa9837090009a835283491a9aa84d8080091199aa9838890009a836a83611a9aa84f0080091199ab9a3370e900000084e0084d808008008a8020a99a984a0099ab9c49102533300095011500410950113535501e00522253353097013333355027253335301400113374a90001bb14984cdd2a40046ec52613374a90021bb149800c008c8cd400541d141d4488cc008cd40ac01cccc124128018cd4078034c07c04400403c4264044cd5ce249025335000980113535501a0012225335309301333335502325301d00100300200100b109501133573892010253340009401133573892010253360008f0113530220052235302d002222222222253353508b013303000a00b2135303a0012235303e0012220021350a10135309d0133573892010253300009e01498cccd5403488d4d404c008894ccd4c02400c54ccd4c01400854ccd4c02400c541f04d41f4cd542400554034cd405801c004541f054ccd4c02400c4d41f4cd542400554034cd4058020004541f0541f0541f054ccd4c01400854ccd4c02400c541f04d41f4cd542400554034cd405801c004541f054ccd4c02400c4d41f4cd542400554034cd4058020004541f0541f0541f04d41f4cd542400554034cd4058019419894ccd4c008004421c04421c044220048882280541e0488800c488800848880048004488800c48880084888004800444ccd5401d416541654164494cd4d41b8004848cd4168cd5421404d4c03000888004cd4168cd54214040052002505b505b12505a235355081013530100012235301b00222222222225335350793301e00a00b213530280012235302c00122235303100322335308701002230930116253353508201004213355098010020011309301161308a01162200211222212333300100500400300211200120011122212333001004003002112001122123300100300212001221233001003002200111222225335307533355304f120013504b504a235300b002223301500200300415335307533355304f120013504b504a235300b002223530160022222222222353501500d22533530840133355305e120013505450562353025001223304b00200400c10860113357389201024c30000850100315335307533355304f120013504b504a235300b002223530160022222222222353501300d22533530840133355305e12001350545056235302700122253353507a00121533530890133305108501003006153353507b330623019007009213308501001002108a01108a011089015335350763301b00c00d2135302500122353029001222333553055120012235302e00222235303300822353035005225335309301333308401004003002001133506f0090081008506701113508c01353088013357389201024c6600089014984218044cd5ce2481024c3100085010021077150741507415074122123300100300212001122123300100300212001221233001003002200122533335300300121505f21505f21505f2133355304612001504a235300d001225335306f3303300200413506300315062003212222300400521222230030052122223002005212222300100520013200135506c22233333333333353019001235300500322222222225335307153353506333355304b12001504f253353072333573466e3c0300041d01cc4d41980045419400c841d041c841cc4cd5ce249024c340007222353006004222222222253353506453353506433355304c1200150502353550790012253353075333573466e3c00803c1dc1d84d41a400c541a000884d419cd4d541e40048800454194854cd4c1ccccd5cd19baf00100c0750741075150701506f235300500322222222225335307133355304b120013504150432333573466ebc0300041d01cccd54c108480048d4d541e00048800400841cc4cd5ce249024c320007222225335306a333573466e1cd4c0200188888888888ccc09801c0380300041b01ac41b04cd5ce2481024c390006b22235300700522222222225335307333355304d1200135043504523530160012225335350690012153353078333040074003010153353506a35301601422222222223305b01b0022153353079333573466e3c0040081ec1e84d4c07401488cccc1b0008004c1d005541b841e841e441e441e002441d44cd5ce249024c6200074225335306833303002f0013335530331200150175045353006004222222222233355303d120012235301600222235301b00322335307100225335307a333573466e3c0500041f01ec4cd415801401c401c801d413c02441a84cd5ce2481024c610006925335306733302f02e001353005003222222222233355304b12001501f235301400122200200910691335738921024c36000682533530673335530411200135037503923300500400100110691335738921024c640006825335306733302f02e001353005003222222222233355304b12001501f23530120012235301600122200200a106913357389201024c35000682353005003222222222253353506333355304b12001504f235301200122533530743303800200e1350680031506700a213530120012235301600122253353506900121507610791506f22353006004222222222253353506433355304c120015050235301300122533530753303900200f1350690031506800a2107513357389201024c380007323530050032222222222353503100b22353503500222353503500822353503900222533530793333333222222253335306d33350640070060031533530800100215335308001005133350610070010041081011333506100700100410810113335061007001004333333335064075225335307b333573466e1c0080041f41f041ac54cd4c1ecccd5cd19b8900200107d07c1069106a22333573466e200080041f41f010088ccd5cd19b8900200107c07d22333573466e200080041f01f4894cd4c1ecccd5cd19b8900200107d07c10011002225335307b333573466e240080041f41f04008400401801401c00800400c41ec4cd5ce249024c330007a222222222212333333333300100b00a009008007006005004003002200122123300100300220012221233300100400300220012212330010030022001212222222300700822122222223300600900821222222230050081222222200412222222003221222222233002009008221222222233001009008200113350325001502f13001002222335530241200123535505a00122335505d002335530271200123535505d001223355060002333535502500123300a4800000488cc02c0080048cc02800520000013301c00200122337000040024446464600200a640026aa0b64466a6a05e0029000111a9aa82e00111299a982c199ab9a3371e0040120b40b22600e0022600c006640026aa0b44466a6a05c0029000111a9aa82d80111299a982b999ab9a3371e00400e0b20b020022600c00642444444444444601801a4424444444444446601601c01a42444444444444601401a44442444444444444666601202001e01c01a444244444444444466601001e01c01a4424444444444446600e01c01a42444444444444600c01a42444444444444600a01a42444444444444600801a42444444444444600601a4424444444444446600401c01a42444444444444600201a400224424660020060042400224424660020060042400244a66a607c666ae68cdc79a9801801110011a98018009100102001f8999ab9a3370e6a6006004440026a60060024400208007e207e442466002006004400244666ae68cdc480100081e81e111199aa980a890009a808a80811a9aa82100091199aa980c090009a80a280991a9aa82280091199a9aa8068009198052400000244660160040024660140029000000998020010009119aa98050900091a9aa8200009119aa821801199a9aa804000919aa98070900091a9aa8220009119aa8238011aa80780080091199aaa80401c801000919aa98070900091a9aa8220009119aa8238011aa806800800999aaa80181a001000888911199aa980209000a80a99aa98050900091a9aa8200009119aa8218011aa805800999aa980209000911a9aa82080111299a981e999aa980b890009a806a80791a9aa82200091198050010028030801899a80c802001a80b00099aa98050900091a9aa820000911919aa8220019800802990009aa82291299a9a80c80089aa8058019109a9aa82300111299a982119806001004099aa80800380089803001801190009aa81f1108911299a9a80a800880111099802801199aa980389000802802000889091118018020891091119801002802089091118008020890008919a80891199a9a803001910010010009a9a80200091000990009aa81c110891299a9a8070008a80811099a808980200119aa980309000802000899a80111299a981800108190800817891091980080180109000899a80191299a9816801080088170168919a80591199a9a802001910010010009a9a8010009100089109198008018010900091299a9a80d999aa980189000a80391a9aa81800091299a9816199ab9a3375e00200a05c05a26a0400062a03e002426a03c6a6aa060002440042a038640026aa05e4422444a66a6a00c00226a6a01400644002442666a6a01800a440046008004666aa600e2400200a00800222440042442446600200800624002266a00444a66a6a02c004420062002a02a24424660020060042400224446a6a008004446a6a00c00644a666a6026666a01400e0080042a66a604c00620022050204e2050244246600200600424002244464646464a666a6a01000c42a666a6a01200c42a666a6a0140104260082c260062c2a666a6a01400e4260082c260062c202a20262a666a6a01200e4260082c260062c2a666a6a01200c4260082c260062c20282a666a6a01000a42024202620222a666a6a01000a42a666a6a01200e42600a2c260082c2a666a6a01200c42600a2c260082c202820242a666a6a01000c42600a2c260082c2a666a6a01000a42600a2c260082c20264a666a6a01000a42a666a6a01200e42a666a6a01400e42666a01e014004002260222c260222c260202c20262a666a6a01000c42a666a6a01200c42666a01c012004002260202c260202c2601e2c202420224a666a6a00e00842a666a6a01000c42a666a6a01200c42666a01c012004002260202c260202c2601e2c20242a666a6a00e00a42a666a6a01000a42666a01a0100040022601e2c2601e2c2601c2c202220204a666a6a00c00642a666a6a00e00a42a666a6a01000a42666a01a0100040022601e2c2601e2c2601c2c20222a666a6a00c00842a666a6a00e00842666a01800e0040022601c2c2601c2c2601a2c2020201e4a666a6a00a00442a666a6a00c00842a666a6a00e00842666a01800e0040022601c2c2601c2c2601a2c20202a666a6a00a00642a666a6a00c00642666a01600c0040022601a2c2601a2c260182c201e201c2424446006008224440042244400224002246a6a0040024444444400e244444444246666666600201201000e00c00a008006004240024c244400624440042444002400244446466a601800a466a601a0084a66a602c666ae68cdc780100080c00b8a801880b900b919a9806802100b9299a980b199ab9a3371e00400203002e2a006202e2a66a6a00a00642a66a6a00c0044266a6014004466a6016004466a601e004466a60200044660280040024034466a6020004403446602800400244403444466a601a0084034444a66a6036666ae68cdc380300180e80e0a99a980d999ab9a3370e00a00403a03826602e00800220382038202a2a66a6a00a0024202a202a2424460040062244002240024244600400644424466600200a00800640024244600400642446002006400244666ae68cdc780100080480411199ab9a3370e00400201000e266ae712401024c630000413357389201024c370000313357389201024c64000021220021220012001235006353002335738921024c6700003498480048004448848cc00400c008448004498448c8c00400488cc00cc0080080050482d87a80d87980f5f6"
        ]
    , inputs = toCardanoUTxO $
        Write.utxoFromTxOutsInRecentEra RecentEraBabbage
        [ ( Write.unsafeMkTxIn tid 0
          , TxOutInRecentEra
              (Write.unsafeAddressFromBytes $ unsafeFromHex $ mconcat
                  [ "714d72cf569a339a18a7d93023139"
                  , "83f56e0d96cd45bdcb1d6512dca6a"
                  ])
              (Convert.toLedgerTokenBundle
                  $ W.TokenBundle.fromCoin $ W.Coin 2_000_000)
              (Write.DatumHash
                  $ fromJust
                  $ Write.datumHashFromBytes
                  $ unsafeFromHex
                  $ mconcat
                      [ "923918e403bf43c34b4ef6b48eb2ee04"
                      , "babed17320d8d1b9ff9ad086e86f44ec"
                      ])
              Nothing
          )
        ]
    , redeemers =
        [ RedeemerSpending (unsafeFromHex "D87A80") (W.TxIn (W.Hash tid) 0)
        ]
    }
  where
    tid = B8.replicate 32 '1'

-- | A collection of signed transaction bytestrings useful for testing.
--
-- These bytestrings can be regenerated by running the integration tests
-- with lib/wallet/test/data/signedTxs/genData.patch applied.
--
signedTxTestData :: IO [(FilePath, ByteString)]
signedTxTestData = do
    let dir = $(getTestData) </> "signedTxs"
    files <- listDirectory dir
    fmap (sortOn (goldenIx . fst) . catMaybes) . forM files $ \name ->
        if ".cbor" `isSuffixOf` name
        then Just . (name,) <$> BS.readFile (dir </> name)
        else pure Nothing
  where
    goldenIx :: FilePath -> Maybe Int
    goldenIx = readMaybe . takeWhile isDigit

testParameter_coinsPerUTxOWord_Alonzo :: Ledger.CoinPerWord
testParameter_coinsPerUTxOWord_Alonzo
    = Ledger.CoinPerWord $ Ledger.Coin 34_482

testParameter_coinsPerUTxOByte_Babbage :: Ledger.CoinPerByte
testParameter_coinsPerUTxOByte_Babbage
    = Ledger.CoinPerByte $ Ledger.Coin 4_310

testStdGenSeed :: StdGenSeed
testStdGenSeed = StdGenSeed 0

--------------------------------------------------------------------------------
-- Arbitrary instances, generators, and shrinkers
--------------------------------------------------------------------------------

instance Arbitrary AnyRecentEra where
    arbitrary = elements
        [ AnyRecentEra RecentEraBabbage
        , AnyRecentEra RecentEraConway
        ]

instance IsCardanoEra era => Arbitrary (Cardano.AddressInEra era) where
    arbitrary = genAddressInEra Cardano.cardanoEra

instance IsCardanoEra era => Arbitrary (Cardano.TxOutDatum ctx era) where
    arbitrary = genTxOutDatum Cardano.cardanoEra

instance Arbitrary Cardano.NetworkId where
    arbitrary = oneof
        [ pure Cardano.Mainnet
        , Cardano.Testnet . Cardano.NetworkMagic <$> arbitrary
        ]

instance IsCardanoEra era => Arbitrary (Cardano.TxOut ctx era) where
    arbitrary = genTxOut Cardano.cardanoEra
    shrink (Cardano.TxOut addr val dat refScript) = tail
        [ Cardano.TxOut addr' val' dat' refScript'
        | addr' <- prependOriginal shrink addr
        , val' <- prependOriginal shrink val
        , dat' <- prependOriginal shrink dat
        , refScript' <- prependOriginal (const []) refScript
        ]

-- NOTE: We should constrain by @IsRecentEra era@ instead, where @RecentEra@ is
-- the two latest eras.
instance IsCardanoEra era => Arbitrary (Cardano.TxOutValue era) where
    arbitrary = case Cardano.cardanoEra @era of
       Cardano.AlonzoEra ->
           Cardano.TxOutValue Cardano.MultiAssetInAlonzoEra
               <$> genValueForTxOut
       Cardano.BabbageEra ->
           Cardano.TxOutValue Cardano.MultiAssetInBabbageEra
               <$>  genValueForTxOut
       e -> error $ mconcat
           [ "Arbitrary (TxOutValue "
           , show e
           , ") not implemented)"
           ]

    shrink (Cardano.TxOutValue Cardano.MultiAssetInAlonzoEra val) =
        map
            (Cardano.TxOutValue Cardano.MultiAssetInAlonzoEra
                . walletToCardanoValue)
            (shrink $ cardanoToWalletValue val)

    shrink (Cardano.TxOutValue Cardano.MultiAssetInBabbageEra val) =
        map
            (Cardano.TxOutValue Cardano.MultiAssetInBabbageEra
                . walletToCardanoValue)
            (shrink $ cardanoToWalletValue val)
    shrink _ =
        error "Arbitrary (TxOutValue era) is not implemented for old eras"

-- Coins (quantities of lovelace) must be strictly positive when included in
-- transactions.
--
instance Arbitrary W.Coin where
    arbitrary = W.genCoinPositive
    shrink = W.shrinkCoinPositive

instance Arbitrary FeePerByte where
    arbitrary = frequency
        [ (1, pure mainnetFeePerByte)
        , (7, FeePerByte <$> arbitrarySizedNatural)
        ]
    shrink (FeePerByte x) =
        FeePerByte <$> shrinkNatural x

instance Arbitrary (W.Hash "Tx") where
    arbitrary = do
        bs <- vectorOf 32 arbitrary
        pure $ W.Hash $ BS.pack bs

instance Arbitrary (Index 'WholeDomain depth) where
    arbitrary = arbitraryBoundedEnum
    shrink = shrinkBoundedEnum

instance Arbitrary (PartialTx Cardano.BabbageEra) where
    arbitrary = do
        let era = BabbageEra
        tx <- genTxForBalancing era
        let (Cardano.Tx (Cardano.TxBody content) _) = tx
        let inputs = Cardano.txIns content
        inputUTxO <- fmap (Cardano.UTxO . Map.fromList) . forM inputs $ \i -> do
            -- NOTE: genTxOut does not generate quantities larger than
            -- `maxBound :: Word64`, however users could supply these.
            -- We should ideally test what happens, and make it clear what code,
            -- if any, should validate.
            o <- genTxOut Cardano.BabbageEra
            return (fst i, o)
        let redeemers = []
        return $ PartialTx tx inputUTxO redeemers
    shrink (PartialTx tx inputUTxO redeemers) =
        [ PartialTx tx inputUTxO' redeemers
        | inputUTxO' <- shrinkInputResolution inputUTxO
        ] <>
        [ restrictResolution $ PartialTx tx' inputUTxO redeemers
        | tx' <- shrinkTxBabbage tx
        ]

instance Arbitrary StdGenSeed  where
    arbitrary = StdGenSeed . fromIntegral @Int <$> arbitrary

instance Arbitrary W.TokenBundle where
    arbitrary = W.genTokenBundleSmallRange
    shrink = W.shrinkTokenBundleSmallRange

instance Arbitrary (TxBalanceSurplus W.Coin) where
    -- We want to test cases where the surplus is zero. So it's important that
    -- we do not restrict ourselves to positive coins here.
    arbitrary = TxBalanceSurplus <$> frequency
        [ (8, W.genCoin)
        , (4, W.genCoin & scale (* (2 `power`  4)))
        , (2, W.genCoin & scale (* (2 `power`  8)))
        , (1, W.genCoin & scale (* (2 `power` 16)))
        ]
    shrink = shrinkMapBy TxBalanceSurplus unTxBalanceSurplus W.shrinkCoin

instance Arbitrary (TxFeeAndChange [W.TxOut]) where
    arbitrary = do
        fee <- W.genCoin
        change <- frequency
            [ (1, pure [])
            , (1, (: []) <$> TxOutGen.genTxOut)
            , (6, listOf TxOutGen.genTxOut)
            ]
        pure $ TxFeeAndChange fee change
    shrink (TxFeeAndChange fee change) =
        uncurry TxFeeAndChange <$> liftShrink2
            (W.shrinkCoin)
            (shrinkList TxOutGen.shrinkTxOut)
            (fee, change)

instance Arbitrary W.TxIn where
    arbitrary = do
        ix <- scale (`mod` 3) arbitrary
        txId <- arbitrary
        pure $ W.TxIn txId ix

instance Arbitrary W.TxOut where
    arbitrary =
        W.TxOut addr <$> scale (`mod` 4) W.genTokenBundleSmallRange
      where
        addr = W.Address $ BS.pack (1:replicate 56 0)
    shrink (W.TxOut addr bundle) =
        [ W.TxOut addr bundle'
        | bundle' <- W.shrinkTokenBundleSmallRange bundle
        ]

instance Arbitrary Wallet' where
    arbitrary = oneof
        [ Wallet' AllKeyPaymentCredentials
            <$> genWalletUTxO genShelleyVkAddr
            <*> pure dummyShelleyChangeAddressGen

        , Wallet' AllByronKeyPaymentCredentials
            <$> genWalletUTxO genByronVkAddr
            <*> pure dummyByronChangeAddressGen
        ]
      where
        genShelleyVkAddr :: Gen (Cardano.AddressInEra Cardano.BabbageEra)
        genShelleyVkAddr = Cardano.shelleyAddressInEra
            <$> (Cardano.makeShelleyAddress
                <$> genNetworkId
                <*> genPaymentCredential -- only vk credentials
                <*> genStakeAddressReference)

        genByronVkAddr :: Gen (Cardano.AddressInEra Cardano.BabbageEra)
        genByronVkAddr = Cardano.byronAddressInEra <$> genAddressByron

        genWalletUTxO genAddr = scale (* 2) $
            W.UTxO . Map.fromList <$> listOf genEntry
          where
            genEntry = (,) <$> genIn <*> genOut
              where
                genIn :: Gen W.TxIn
                genIn = W.genTxIn

                genOut :: Gen W.TxOut
                genOut = cardanoToWalletTxOut <$>
                  (Cardano.TxOut
                        <$> genAddr
                        <*> (scale (* 2) (genTxOutValue era))
                        <*> (pure Cardano.TxOutDatumNone)
                        <*> (pure Cardano.ReferenceScriptNone))
                  where
                    era = Cardano.BabbageEra

    shrink (Wallet' utxoAssumptions utxo changeAddressGen) =
        [ Wallet' utxoAssumptions utxo' changeAddressGen
        | utxo' <- shrinkUTxO utxo
        ]
      where
        -- We cannot use 'Cardano.Wallet.Primitive.Types.UTxO.Gen.shrinkUTxO'
        -- because it will shrink to invalid addresses.
        shrinkUTxO
            = take 16
            . fmap (W.UTxO . Map.fromList)
            . shrinkList shrinkEntry
            . Map.toList
            . W.unUTxO

        shrinkEntry _ = []

-- | For writing shrinkers in the style of https://stackoverflow.com/a/14006575
prependOriginal :: (t -> [t]) -> t -> [t]
prependOriginal shrinker x = x : shrinker x

shrinkFee :: Ledger.Coin -> [Ledger.Coin]
shrinkFee (Ledger.Coin 0) = []
shrinkFee _ = [Ledger.Coin 0]

shrinkInputResolution
    :: forall era.
        ( Arbitrary (Cardano.TxOut Cardano.CtxUTxO era)
        )
    => Cardano.UTxO era
    -> [Cardano.UTxO era]
shrinkInputResolution =
    shrinkMapBy utxoFromList utxoToList shrinkUTxOEntries
   where
     utxoToList (Cardano.UTxO u) = Map.toList u
     utxoFromList = Cardano.UTxO . Map.fromList

     -- NOTE: We only want to shrink the outputs, keeping the inputs and length
     -- of the list the same.
     shrinkUTxOEntries :: Arbitrary o => [(i, o)] -> [[(i, o)]]
     shrinkUTxOEntries ((i,o) : rest) = mconcat
         -- First shrink the first element
         [ map (\o' -> (i, o') : rest ) (shrink o)
         -- Recurse to shrink subsequent elements on their own
         , map ((i,o):) (shrinkUTxOEntries rest)
         ]
     shrinkUTxOEntries [] = []

shrinkScriptData
    :: Era (Cardano.ShelleyLedgerEra era)
    => Cardano.TxBodyScriptData era
    -> [Cardano.TxBodyScriptData era]
shrinkScriptData Cardano.TxBodyNoScriptData = []
shrinkScriptData (Cardano.TxBodyScriptData era
    (Alonzo.TxDats dats) (Alonzo.Redeemers redeemers)) = tail
        [ Cardano.TxBodyScriptData era
            (Alonzo.TxDats dats')
            (Alonzo.Redeemers redeemers')
        | dats' <- dats :
            (Map.fromList <$> shrinkList (const []) (Map.toList dats))
        , redeemers' <- redeemers :
            (Map.fromList <$> shrinkList (const []) (Map.toList redeemers))
        ]

shrinkSeq :: Foldable t => (a -> [a]) -> t a -> [StrictSeq.StrictSeq a]
shrinkSeq shrinkElem =
    map StrictSeq.fromList . shrinkList shrinkElem . F.toList

shrinkSet :: Ord a => (a -> [a]) -> Set a -> [Set a]
shrinkSet shrinkElem = map Set.fromList . shrinkList shrinkElem . F.toList

shrinkStrictMaybe :: StrictMaybe a -> [StrictMaybe a]
shrinkStrictMaybe = \case
    SNothing -> []
    SJust _ -> [SNothing]

shrinkTxBabbage
    :: Cardano.Tx Cardano.BabbageEra
    -> [Cardano.Tx Cardano.BabbageEra]
shrinkTxBabbage (Cardano.Tx bod wits) =
    [ Cardano.Tx bod' wits | bod' <- shrinkTxBodyBabbage bod ]

shrinkTxBodyBabbage
    :: Cardano.TxBody Cardano.BabbageEra
    -> [Cardano.TxBody Cardano.BabbageEra]
shrinkTxBodyBabbage (Cardano.ShelleyTxBody e bod scripts scriptData aux val) =
    tail
        [ Cardano.ShelleyTxBody e bod' scripts' scriptData' aux' val'
        | bod' <- prependOriginal shrinkLedgerTxBody bod
        , aux' <- aux : filter (/= aux) [Nothing]
        , scriptData' <- prependOriginal shrinkScriptData scriptData
        , scripts' <- prependOriginal (shrinkList (const [])) scripts
        , val' <- case Cardano.txScriptValiditySupportedInShelleyBasedEra e of
            Nothing -> [val]
            Just txsvsie -> val : filter (/= val)
                [ Cardano.TxScriptValidity txsvsie Cardano.ScriptValid ]
        ]
  where
    shrinkLedgerTxBody
        :: Ledger.TxBody (Cardano.ShelleyLedgerEra Cardano.BabbageEra)
        -> [Ledger.TxBody (Cardano.ShelleyLedgerEra Cardano.BabbageEra)]
    shrinkLedgerTxBody body = tail
        [ body
            & withdrawalsTxBodyL .~ wdrls'
            & outputsTxBodyL .~ outs'
            & inputsTxBodyL .~ ins'
            & certsTxBodyL .~ certs'
            & mintTxBodyL .~ mint'
            & reqSignerHashesTxBodyL .~ rsh'
            & updateTxBodyL .~ updates'
            & feeTxBodyL .~ txfee'
            & vldtTxBodyL .~ vldt'
            & scriptIntegrityHashTxBodyL .~ adHash'
        | wdrls' <- prependOriginal shrinkWdrl
            (body ^. withdrawalsTxBodyL)
        , outs' <- prependOriginal (shrinkSeq (const []))
            (body ^. outputsTxBodyL)
        , ins' <- prependOriginal (shrinkSet (const []))
            (body ^. inputsTxBodyL)
        , certs' <- prependOriginal (shrinkSeq (const []))
            (body ^. certsTxBodyL)
        , mint' <- prependOriginal shrinkValue
            (body ^. mintTxBodyL)
        , rsh' <- prependOriginal (shrinkSet (const []))
            (body ^. reqSignerHashesTxBodyL)
        , updates' <- prependOriginal shrinkStrictMaybe
            (body ^. updateTxBodyL)
        , txfee' <- prependOriginal shrinkFee
            (body ^. feeTxBodyL)
        , vldt' <- prependOriginal shrinkValidity
            (body ^. vldtTxBodyL)
        , adHash' <- prependOriginal shrinkStrictMaybe
            (body ^. scriptIntegrityHashTxBodyL)
        ]

    shrinkValidity (ValidityInterval a b) = tail
        [ ValidityInterval a' b'
        | a' <- prependOriginal shrinkStrictMaybe a
        , b' <- prependOriginal shrinkStrictMaybe b
        ]

shrinkValue :: (Eq a, Monoid a) => a -> [a]
shrinkValue v = filter (/= v) [mempty]

shrinkWdrl :: Withdrawals era -> [Withdrawals era]
shrinkWdrl (Withdrawals m) = map (Withdrawals . Map.fromList) $
    shrinkList shrinkWdrl' (Map.toList m)
    where
    shrinkWdrl' (acc, Ledger.Coin c) =
        [ (acc, Ledger.Coin c')
        | c' <- filter (>= 1) $ shrink c
        ]

--------------------------------------------------------------------------------
-- Pretty-printing
--------------------------------------------------------------------------------

-- | A convenient wrapper type that allows values of any type with a 'Buildable'
--   instance to be pretty-printed through the 'Show' interface.
--
newtype ShowBuildable a = ShowBuildable a
    deriving newtype Arbitrary

instance Buildable a => Show (ShowBuildable a) where
    show (ShowBuildable x) = pretty x

instance Buildable UTxOAssumptions where
    build = \case
        AllKeyPaymentCredentials -> "AllKeyPaymentCredentials"
        AllByronKeyPaymentCredentials -> "AllByronKeyPaymentCredentials"
        AllScriptPaymentCredentialsFrom scriptTemplate _scriptLookup ->
            nameF "AllScriptPaymentCredentialsFrom" $
                blockListF [ nameF "scriptTemplate" $ build scriptTemplate ]

instance Buildable AnyChangeAddressGenWithState where
    build (AnyChangeAddressGenWithState (ChangeAddressGen g maxLengthAddr) s0) =
        blockListF
            [ nameF "changeAddr0" $
                build $ show $ fst $ g s0
            , nameF "max address length" $
                build $ BS.length $ serialiseAddr maxLengthAddr
            ]

-- CSV with the columns: wallet_balance,(fee,minfee | error)
instance Buildable BalanceTxGolden where
    build (BalanceTxGoldenFailure c err) = mconcat
        [ build c
        , ","
        , build (T.pack err)
        ]
    build (BalanceTxGoldenSuccess c fee minfee) = mconcat
        [ build c
        , ","
        , lovelaceF fee
        , ","
        , lovelaceF minfee
        ]
      where
        lovelaceF (Cardano.Lovelace l)
            | l < 0     = "-" <> pretty (W.Coin.unsafeFromIntegral (-l))
            | otherwise = pretty (W.Coin.unsafeFromIntegral l)

instance Buildable Wallet' where
    build (Wallet' assumptions utxo changeAddressGen) =
        nameF "Wallet" $ mconcat
            [ nameF "assumptions" $ build assumptions
            , nameF "changeAddressGen" $ build changeAddressGen
            , nameF "utxo" $ pretty utxo
            ]

--------------------------------------------------------------------------------
-- Miscellaneous orphan instances
--------------------------------------------------------------------------------

instance Semigroup (Cardano.UTxO era) where
    Cardano.UTxO a <> Cardano.UTxO b = Cardano.UTxO (a <> b)

instance Monoid (Cardano.UTxO era) where
    mempty = Cardano.UTxO mempty
