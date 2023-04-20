{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2023 IOHK
-- License: Apache-2.0
--
-- Provides protocol parameters.
--
module Cardano.Wallet.Write.ProtocolParameters
    ( ProtocolParameters (..)
    , unsafeFromWalletProtocolParameters
    ) where

import Prelude

import qualified Cardano.Api as CardanoApi
import qualified Cardano.Api.Extra as CardanoApi
import qualified Cardano.Wallet.Primitive.Types as Wallet
import qualified Cardano.Wallet.Write.Tx as WriteTx

-- TODO:
--  - Make this data type abstract: don't export the constructor.
--  - Replace this type with a re-exported 'Ledger.PParams era'.
data ProtocolParameters era = ProtocolParameters
    { pparamsWallet
        :: !Wallet.ProtocolParameters
    , pparamsLedger
        :: !(WriteTx.PParams (WriteTx.ShelleyLedgerEra era))
    }

-- TODO: ADP-2459 - replace with something nicer.
unsafeFromWalletProtocolParameters
    :: forall era. CardanoApi.IsShelleyBasedEra era
    => Wallet.ProtocolParameters
    -> ProtocolParameters era
unsafeFromWalletProtocolParameters pparamsWallet =
    ProtocolParameters {pparamsWallet, pparamsLedger}
  where
    pparamsLedger = maybe
        (error missingNodeParamsError)
        (CardanoApi.unbundleLedgerShelleyBasedProtocolParams
            (CardanoApi.shelleyBasedEra @era)
            . CardanoApi.bundleProtocolParams (CardanoApi.cardanoEra @era)
        )
        (Wallet.currentNodeProtocolParameters pparamsWallet)
    missingNodeParamsError = unwords
        [ "unsafeFromWalletProtocolParameters: no nodePParams."
        , "This should only be possible in Byron, where IsShelleyBasedEra"
        , "should prevent this from being reached."
        ]
