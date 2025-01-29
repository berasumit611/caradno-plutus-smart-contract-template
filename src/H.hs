{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module FoodOrderReward (writeFoodOrderRewardScript, writeTypedFoodOrderRewardScript) where

import Cardano.Api
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Plutus.V2.Ledger.Api
import Cardano.Api.Shelley (PlutusScript (..))
import Prelude (putStrLn, show)
import PlutusTx.Prelude
import Prelude (FilePath, IO)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

-- Simple validator logic for food order rewards
{-# INLINABLE mkValidator #-}
mkValidator :: Integer -> () -> Plutus.V2.Ledger.Api.ScriptContext -> Bool
mkValidator orderAmount _ _ =
    if orderAmount > 1000
    then traceIfTrue "You got a reward!" True
    else traceIfTrue "No reward" False

-- Compile the validator
validator :: Plutus.V2.Ledger.Api.Validator
validator = Plutus.V2.Ledger.Api.mkValidatorScript $
    $$(PlutusTx.compile [|| mkValidator ||])

-- General function to write a compiled Plutus script to a file
writeValidator :: FilePath -> Plutus.V2.Ledger.Api.Validator -> IO (Either (FileError ()) ())
writeValidator file validator = do
    createDirectoryIfMissing True (takeDirectory file)
    result <- writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing $
        PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise $ Plutus.V2.Ledger.Api.unValidatorScript validator
    case result of
        Left err -> putStrLn $ "Failed to write script: " <> show err
        Right () -> putStrLn $ "Successfully wrote script to: " <> file
    return result

-- Writes the untyped "Food Order Reward" script to a file
writeFoodOrderRewardScript :: IO (Either (FileError ()) ())
writeFoodOrderRewardScript = writeValidator "output/food-order-reward.plutus" validator

-- Main function to demonstrate script writing
main :: IO ()
main = do
    result <- writeFoodOrderRewardScript
    case result of
        Left err -> print err
        Right _  -> putStrLn "Food Order Reward script written successfully!"
