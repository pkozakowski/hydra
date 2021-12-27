{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}

module Market.Blockchain where

import Data.ByteString
import Data.Constraint
import Data.Fixed
import Market
import Polysemy
import Polysemy.Error
import Polysemy.Input

class Platform p where

    type Effects p :: EffectRow
    type Wallet p :: *

    loadWallet
        :: Member (Error String) r
        => p -> ByteString -> Sem r (Wallet p)

class Platform p => Exchange p e | e -> p where

    type SwapConfig e :: *

    fetchPrices
        :: Members (Error String : Effects p) r
        => [Asset] -> Sem r Prices
    
    swap
        :: Members
            ( Input e
            : Input (Wallet p)
            : Input (SwapConfig e)
            : Input Prices
            : Error String
            : Effects p
            ) r
        => Asset -> Asset -> Amount -> Sem r ()