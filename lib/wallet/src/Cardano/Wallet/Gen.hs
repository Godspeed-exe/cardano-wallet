{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Shared QuickCheck generators for wallet types.
--
-- Our convention is to let each test module define its own @Arbitrary@ orphans.
-- This module allows for code-reuse where desired, by providing generators.
module Cardano.Wallet.Gen
    ( genMnemonic
    , genPercentage
    , shrinkPercentage
    , genLegacyAddress
    , genBlockHeader
    , genChainPoint
    , genSlot
    , genActiveSlotCoefficient
    , shrinkActiveSlotCoefficient
    , genSlotNo
    , shrinkSlotNo
    , genScript
    , genScriptCosigners
    , genScriptTemplate
    , genMockXPub
    , genNatural
    , genWalletId
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPub
    , xpubFromBytes
    )
import Cardano.Address.Script
    ( Cosigner (..)
    , Script (..)
    , ScriptTemplate (..)
    )
import Cardano.Mnemonic
    ( ConsistentEntropy
    , EntropySize
    , Mnemonic
    , entropyToMnemonic
    )
import Cardano.Wallet.Address.Discovery.Shared
    ( retrieveAllCosigners
    )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , BlockHeader (..)
    , ChainPoint (..)
    , Slot
    , SlotNo (..)
    , WalletId (..)
    , WithOrigin (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..)
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Cardano.Wallet.Primitive.Types.ProtocolMagic
    ( ProtocolMagic (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex
    , unsafeMkEntropy
    , unsafeMkPercentage
    )
import Control.Monad
    ( replicateM
    )
import Crypto.Hash
    ( hash
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Quantity
    ( Percentage (..)
    , Quantity (..)
    )
import Data.Ratio
    ( denominator
    , numerator
    , (%)
    )
import Data.Word
    ( Word32
    )
import GHC.TypeLits
    ( natVal
    )
import Numeric.Natural
    ( Natural
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Positive (..)
    , arbitrarySizedNatural
    , choose
    , elements
    , frequency
    , oneof
    , scale
    , sized
    , sublistOf
    , suchThat
    , vector
    , vectorOf
    )

import qualified Cardano.Byron.Codec.Cbor as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString as BS
import qualified Data.Map as Map

-- | Generates an arbitrary mnemonic of a size according to the type parameter.
--
-- E.g:
-- >>> arbitrary = SomeMnemonic <$> genMnemonic @12
genMnemonic
    :: forall mw ent csz.
     ( ConsistentEntropy ent mw csz
     , EntropySize mw ~ ent
     )
    => Gen (Mnemonic mw)
genMnemonic = do
        let n = fromIntegral (natVal $ Proxy @(EntropySize mw)) `div` 8
        bytes <- BS.pack <$> vector n
        let ent = unsafeMkEntropy @(EntropySize mw) bytes
        return $ entropyToMnemonic ent

genPercentage :: Gen Percentage
genPercentage = unsafeMkPercentage . fromRational . toRational <$> genDouble
  where
    genDouble :: Gen Double
    genDouble = choose (0, 1)

shrinkPercentage :: Percentage -> [Percentage]
shrinkPercentage x = unsafeMkPercentage <$>
    ((% q) <$> shrink p) ++ (map (p %) . filter (/= 0) $ shrink q)
  where
    p = numerator $ getPercentage x
    q = denominator $ getPercentage x

genLegacyAddress :: Maybe ProtocolMagic -> Gen Address
genLegacyAddress pm = do
    bytes <- BS.pack <$> vector 64
    case xpubFromBytes bytes of
        Nothing -> error "genLegacyAddress: xpubFromBytes failed"
        Just key ->
            pure $ Address
                 $ CBOR.toStrictByteString
                 $ CBOR.encodeAddress key
                 $ maybe [] (pure . CBOR.encodeProtocolMagicAttr) pm

--
-- Slotting
--


-- | Don't generate /too/ large slots
genSlotNo :: Gen SlotNo
genSlotNo = SlotNo . fromIntegral <$> arbitrary @Word32

shrinkSlotNo :: SlotNo -> [SlotNo]
shrinkSlotNo (SlotNo x) = map SlotNo $ shrink x

genChainPoint :: Gen ChainPoint
genChainPoint = frequency
    [ ( 1, pure ChainPointAtGenesis)  -- "common" but not "very common"
    , (40, toChainPoint <$> (genBlockHeader =<< genSlotNo))
    ]
  where
    toChainPoint (BlockHeader slot _ h _) = ChainPoint slot h

genSlot :: Gen Slot
genSlot = frequency
    [ ( 1, pure Origin)
    , (40, At <$> genSlotNo)
    ]

genBlockHeader :: SlotNo -> Gen BlockHeader
genBlockHeader sl = do
        BlockHeader sl (mockBlockHeight sl) <$> genHash <*> (Just <$> genHash)
      where
        mockBlockHeight :: SlotNo -> Quantity "block" Word32
        mockBlockHeight = Quantity . fromIntegral . unSlotNo

        genHash = elements
            [ Hash $ unsafeFromHex
                "aac1308b9868af89c396b08ff6f3cfea8e0859c94d1b3bc834baeaaff8645448"
            , Hash $ unsafeFromHex
                "d93b27cc7bb6fd2fe6ee42de5328c13606bb714a78475a41335207d2afd6026e"
            , Hash $ unsafeFromHex
                "63b8828e2eadc3f14b9b691fa9df76139a9c9b13a12ec862b324cc5a88f9fcc5"
            ]

genActiveSlotCoefficient :: Gen ActiveSlotCoefficient
genActiveSlotCoefficient = ActiveSlotCoefficient <$> choose (0.001, 1.0)

shrinkActiveSlotCoefficient :: ActiveSlotCoefficient -> [ActiveSlotCoefficient]
shrinkActiveSlotCoefficient (ActiveSlotCoefficient f)
        | f < 1 = [1]
        | otherwise = []

genNatural :: Gen Natural
genNatural = arbitrarySizedNatural

genScript :: [a] -> Gen (Script a)
genScript elems = scale (`div` 3) $ sized scriptTree
    where
        scriptTree 0 = oneof
            [ RequireSignatureOf <$> elements elems
            , ActiveFromSlot <$> genNatural
            , ActiveUntilSlot <$> genNatural
            ]
        scriptTree n = do
            Positive m <- arbitrary
            let n' = n `div` (m + 1)
            scripts' <- vectorOf m (scriptTree n')
            atLeast <- choose (1, fromIntegral m)
            elements
                [ RequireAllOf scripts'
                , RequireAnyOf scripts'
                , RequireSomeOf atLeast scripts'
                ]

genScriptCosigners :: Gen (Script Cosigner)
genScriptCosigners = do
    numOfCosigners <- choose (1,10)
    genScript $ Cosigner <$> [0..numOfCosigners]

genScriptTemplate :: Gen ScriptTemplate
genScriptTemplate = do
    script <- genScriptCosigners `suchThat` (not . null . retrieveAllCosigners)
    let scriptCosigners = retrieveAllCosigners script
    cosignersSubset <- sublistOf scriptCosigners `suchThat` (not . null)
    xpubs <- vectorOf (length cosignersSubset) genMockXPub
    pure $ ScriptTemplate (Map.fromList $ zip cosignersSubset xpubs) script

genMockXPub :: Gen XPub
genMockXPub = fromMaybe impossible . xpubFromBytes . BS.pack <$> genBytes
  where
    genBytes = vectorOf 64 arbitrary
    impossible = error "incorrect length in genMockXPub"

genWalletId :: Gen WalletId
genWalletId = do
    bytes <- BS.pack <$> replicateM 16 arbitrary
    pure $ WalletId (hash bytes)
