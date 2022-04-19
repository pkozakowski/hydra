{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Market.Blockchain.Types where

import Data.Solidity.Prim.Address
import Dhall (FromDhall)
import qualified Dhall as Dh
import Network.Ethereum.Unit

instance FromDhall Shannon where
    autoWith _
        = Dh.record
        $ Dh.field "gwei"
        $ fromIntegral <$> Dh.natural

instance FromDhall Address where
    autoWith _ = Address . fromIntegral <$> Dh.natural
