{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module FoodOrderReward where

import Plutus.V2.Ledger.Api      (BuiltinData, Validator, mkValidatorScript, ScriptContext, unValidatorScript)
import PlutusTx                 (compile, toBuiltinData, unsafeFromBuiltinData)
import PlutusTx.Prelude         (traceIfFalse, (>=), error, Either(Left), Either(Right), Either, Bool(False), Bool(True), Maybe(Nothing), (.), (&&))
import Prelude                  (IO, FilePath, writeFile, show, Bool, Integer, print, (++), String, ($), putStrLn, undefined)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Cardano.Api
import Codec.Serialise (serialise)
import Cardano.Api.Shelley (PlutusScript (..), writeFileTextEnvelope, displayError)
import qualified Plutus.V2.Ledger.Api


secretOrder :: Integer
secretOrder = 1000

-- | Validator script logic: Checks if the order amount is >= 1000 and rewards accordingly.
{-# INLINABLE validateOrder #-}
validateOrder :: BuiltinData -> BuiltinData -> BuiltinData -> ()
validateOrder _ redeemer _ =
    let
        orderAmount = unsafeFromBuiltinData @Integer redeemer
    in
        if traceIfFalse "Not Got reward" (orderAmount >= secretOrder)
            then () 
            else error ()  -- This ensures the function always returns ().

-- | Compile the validator into a Plutus script.
validator :: Validator
validator = mkValidatorScript $$(compile [|| validateOrder ||])

-- | Write the compiled script to a file for deployment.
writePlutusScript :: FilePath -> Validator -> IO ()
writePlutusScript file validator = do 
    createDirectoryIfMissing True (takeDirectory file)
    result <- writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing $
        PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise $ Plutus.V2.Ledger.Api.unValidatorScript 
        validator
    case result of 
        Left err -> print $ displayError err 
        Right () -> putStrLn "Successfully wrote Plutus script to file."

writeScript :: IO ()
writeScript = writePlutusScript "scripts4/foodOrderReward.plutus" validator
