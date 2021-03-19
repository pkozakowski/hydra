module Market.Error
    ( ErrorM
    ) where

import Control.Monad.Except
import Market.Types

data Error 
    = UnsupportedAsset Asset
    | OtherError String

type ErrorM = Except Error
