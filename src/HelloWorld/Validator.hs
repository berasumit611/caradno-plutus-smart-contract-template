-- AlwaysSucceeds/Validator.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module HelloWorld.Validator (validator) where

import Plutus.V2.Ledger.Api (Validator, mkValidatorScript)
import PlutusTx
import PlutusTx.Prelude
import Prelude ()


-- Define a simple validator that does nothing but succeed
{-# INLINABLE helloWorldValidator #-}
helloWorldValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
helloWorldValidator _ _ _ = ()

-- Compile the helloWorldValidator to a Plutus Validator
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| helloWorldValidator ||])
