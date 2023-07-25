{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
module Cardano.Wallet.Read.Primitive.Tx.Byron
  ( fromTxAux
  )
where

import Cardano.Chain.UTxO
  ( ATxAux (..)
  , Tx (..)
  , taTx
  )
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import Cardano.Wallet.Read.Eras
  ( byron
  , inject
  )
import Cardano.Wallet.Read.Primitive.Tx.Features.Inputs
  ( fromByronTxIn
  )
import Cardano.Wallet.Read.Primitive.Tx.Features.Outputs
  ( fromByronTxOut
  )
import Cardano.Wallet.Read.Tx
  ( Tx (..)
  )
import Cardano.Wallet.Read.Tx.CBOR
  ( renderTxToCBOR
  )
import Cardano.Wallet.Read.Tx.Hash
  ( byronTxHash
  )
import Control.Monad
  ( void
  )
import qualified Data.List.NonEmpty as NE
import Prelude

fromTxAux :: ATxAux a -> W.Tx
fromTxAux txAux = case taTx txAux of
  UnsafeTx inputs outputs _attributes ->
    W.Tx
      { txId = W.Hash $ byronTxHash txAux
      , txCBOR = Just $ renderTxToCBOR $ inject byron $ Tx $ void txAux
      , fee = Nothing
      , -- TODO: Review 'W.Tx' to not require resolved inputs but only inputs
        resolvedInputs =
          (,Nothing) . fromByronTxIn <$> NE.toList inputs
      , resolvedCollateralInputs = []
      , outputs =
          fromByronTxOut <$> NE.toList outputs
      , collateralOutput =
          Nothing
      , withdrawals =
          mempty
      , metadata =
          Nothing
      , scriptValidity =
          Nothing
      }
