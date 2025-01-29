-- AlwaysSucceeds/TypedValidator.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module HelloWorld.TypedValidator (validator) where

import Plutus.V2.Ledger.Api (Validator, mkValidatorScript)
import PlutusTx
import PlutusTx.Prelude
import Prelude ()

-- Define the same validator function as before
{-# INLINABLE helloWorldValidator #-}
helloWorldValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
helloWorldValidator _ _ _ = ()

-- Compile to a typed validator
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| helloWorldValidator ||])
