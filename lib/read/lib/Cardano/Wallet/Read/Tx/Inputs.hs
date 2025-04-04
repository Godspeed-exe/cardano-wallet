{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Raw inputs data extraction from 'Tx'
--

module Cardano.Wallet.Read.Tx.Inputs
    ( InputsType
    , Inputs (..)
    , getEraInputs
    )
    where

import Prelude

import Cardano.Api
    ( AllegraEra
    , AlonzoEra
    , BabbageEra
    , ByronEra
    , ConwayEra
    , MaryEra
    , ShelleyEra
    )
import Cardano.Ledger.Core
    ( bodyTxL
    , inputsTxBodyL
    )
import Cardano.Ledger.Crypto
    ( StandardCrypto
    )
import Cardano.Wallet.Read.Eras
    ( EraFun (..)
    )
import Cardano.Wallet.Read.Tx
    ( Tx (..)
    )
import Cardano.Wallet.Read.Tx.Eras
    ( onTx
    )
import Control.Lens
    ( (^.)
    )
import Data.List.NonEmpty
    ( NonEmpty
    )
import Data.Set
    ( Set
    )

import qualified Cardano.Chain.UTxO as BY
import qualified Cardano.Ledger.Shelley.API as SH

type family InputsType era where
    InputsType ByronEra = NonEmpty BY.TxIn
    InputsType ShelleyEra = Set (SH.TxIn StandardCrypto)
    InputsType AllegraEra = Set (SH.TxIn StandardCrypto)
    InputsType MaryEra = Set (SH.TxIn StandardCrypto)
    InputsType AlonzoEra = Set (SH.TxIn StandardCrypto)
    InputsType BabbageEra = Set (SH.TxIn StandardCrypto)
    InputsType ConwayEra = Set (SH.TxIn StandardCrypto)

newtype Inputs era = Inputs (InputsType era)

deriving instance Show (InputsType era) => Show (Inputs era)
deriving instance Eq (InputsType era) => Eq (Inputs era)

-- | Extract the inputs from a transaction in any era.
getEraInputs :: EraFun Tx Inputs
getEraInputs =
    EraFun
        { byronFun = onTx $ \tx -> Inputs $ BY.txInputs $ BY.taTx tx
        , shelleyFun = shelleyInputs
        , allegraFun = shelleyInputs
        , maryFun = shelleyInputs
        , alonzoFun = shelleyInputs
        , babbageFun = shelleyInputs
        , conwayFun = shelleyInputs
        }
  where
    shelleyInputs = onTx $ \tx -> Inputs (tx ^. bodyTxL . inputsTxBodyL)
