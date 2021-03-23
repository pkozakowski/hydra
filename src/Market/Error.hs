module Market.Error
    ( ErrorM
    ) where

import Control.Monad.Except
import Market.Types

data Error = OtherError String

type ErrorM = Except Error
