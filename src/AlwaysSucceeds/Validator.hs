{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

-- PlutusV2
module AlwaysSucceeds.Validator (validator) where

import Plutus.V2.Ledger.Api
import PlutusTx

{-# INLINEABLE myValidator #-}
myValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
myValidator _ _ _ = ()

validator :: Validator
validator = mkValidatorScript $$(compile [||myValidator||])
