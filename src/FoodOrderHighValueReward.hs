{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module FoodOrderHighValueReward where

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

-- Threshold for high-value order
highValueOrderThreshold :: Integer
highValueOrderThreshold = 500

-- Reward amount for high-value orders
rewardAmount :: Integer
rewardAmount = 100

{-# INLINABLE validateHighValueOrder #-}
validateHighValueOrder :: BuiltinData -> BuiltinData -> BuiltinData -> ()
validateHighValueOrder _ redeemer _ =
    let 
        orderAmount = unsafeFromBuiltinData @Integer redeemer
    in
        if traceIfFalse "High-value order reward" (orderAmount >= highValueOrderThreshold)
            then ()
            else error ()

-- Compile the validator into a Plutus script
validator :: Validator
validator = mkValidatorScript $$(compile [|| validateHighValueOrder ||])

-- Write the compiled script to a file for deployment
writePlutusScript :: FilePath -> Validator -> IO ()
writePlutusScript file validator = do
    createDirectoryIfMissing True (takeDirectory file)
    result <- writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing $
        PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise $ Plutus.V2.Ledger.Api.unValidatorScript validator
    
    case result of
        Left err -> print $ displayError err
        Right () -> putStrLn "Successfully wrote Plutus script to file."

-- Main function to write the script
writeScript :: IO ()
writeScript = writePlutusScript "scripts3/foodOrderHighValueReward.plutus" validator