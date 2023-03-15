{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Reference input data extraction from 'Tx'
--

module Cardano.Wallet.Read.Tx.ReferenceInputs
    ( ReferenceInputsType
    , ReferenceInputs (..)
    , getEraReferenceInputs
    )
    where

import Prelude

import Cardano.Api
    ( AllegraEra, AlonzoEra, BabbageEra, ByronEra, MaryEra, ShelleyEra )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Wallet.Read.Eras
    ( EraFun (..) )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Read.Tx.Eras
    ( onTx )
import Data.Set
    ( Set )
import GHC.Records
    ( HasField (..) )

import qualified Cardano.Ledger.Alonzo.Tx as AL
import qualified Cardano.Ledger.Shelley.API as SH

type family ReferenceInputsType era where
    ReferenceInputsType ByronEra = ()
    ReferenceInputsType ShelleyEra = ()
    ReferenceInputsType AllegraEra = ()
    ReferenceInputsType MaryEra = ()
    ReferenceInputsType AlonzoEra = ()
    ReferenceInputsType BabbageEra = Set (SH.TxIn StandardCrypto)

newtype ReferenceInputs era = ReferenceInputs (ReferenceInputsType era)

deriving instance Show (ReferenceInputsType era) => Show (ReferenceInputs era)
deriving instance Eq (ReferenceInputsType era) => Eq (ReferenceInputs era)

getEraReferenceInputs :: EraFun Tx ReferenceInputs
getEraReferenceInputs
    = EraFun
        { byronFun = \_ -> ReferenceInputs ()
        , shelleyFun = \_ -> ReferenceInputs ()
        , allegraFun = \_ -> ReferenceInputs ()
        , maryFun = \_ -> ReferenceInputs ()
        , alonzoFun = \_ -> ReferenceInputs ()
        , babbageFun = onTx $ \(AL.AlonzoTx b _ _ _) -> getReferenceInputs b
        }

getReferenceInputs
    :: ( HasField "referenceInputs" a (ReferenceInputsType b))
    => a -> ReferenceInputs b
getReferenceInputs =  ReferenceInputs . getField @"referenceInputs"
