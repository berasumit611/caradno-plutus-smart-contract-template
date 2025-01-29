-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE TypeApplications #-}

-- module SmC.Compiler where

-- import PlutusTx
-- import PlutusTx.Prelude
-- import Plutus.V2.Ledger.Api
-- import Ledger (Validator, mkValidatorScript, unValidatorScript)
-- import qualified PlutusTx
-- import Cardano.Api (PlutusScript (..), writeFileTextEnvelope)
-- import Codec.Serialise (serialise)
-- import qualified Data.ByteString.Lazy as LBS
-- import qualified Data.ByteString.Short as SBS
-- import System.Directory (createDirectoryIfMissing)
-- import System.FilePath (takeDirectory)
-- import Prelude (IO, FilePath, putStrLn, ($), Either, return)

-- -- Define the data types for Datum and Redeemer
-- data OrderDatum = OrderDatum
--     { orderAmount :: Integer }
-- PlutusTx.makeLift ''OrderDatum

-- data OrderRedeemer = Redeem
-- PlutusTx.makeIsDataIndexed ''OrderRedeemer [('Redeem, 0)]

-- -- Validator logic
-- {-# INLINABLE mkValidator #-}
-- mkValidator :: OrderDatum -> OrderRedeemer -> ScriptContext -> Bool
-- mkValidator OrderDatum{..} _ _ =
--     if orderAmount > 5000
--     then traceIfTrue "You got a reward of 100 thousand!" True
--     else traceIfTrue "No reward" False

-- -- Compile the validator
-- validator :: Validator
-- validator = mkValidatorScript $$(compile [|| mkValidator ||])

-- -- Function to write the compiled script to a file
-- writeValidator :: FilePath -> Validator -> IO (Either (FileError ()) ())
-- writeValidator file v = do
--     createDirectoryIfMissing True (takeDirectory file)
--     let script = PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise $ unValidatorScript v
--     writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing script

-- -- Writes the "Food Order Reward" script to a file
-- writeFoodOrderRewardScript :: IO (Either (FileError ()) ())
-- writeFoodOrderRewardScript = writeValidator "output/food-order-reward.plutus" validator

-- -- Main function to demonstrate script writing
-- main :: IO ()
-- main = do
--     result <- writeFoodOrderRewardScript
--     case result of
--         Left err -> print err
--         Right _  -> putStrLn "Food Order Reward script written successfully!"
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TemplateHaskell #-}

-- module SmC.Compiler where

-- import PlutusTx.Prelude
-- import Plutus.V2.Ledger.Api
-- import Ledger (mkValidatorScript, unValidatorScript)
-- import Cardano.Api (PlutusScript (..), writeFileTextEnvelope)
-- import Codec.Serialise (serialise)
-- import qualified Data.ByteString.Lazy as LBS
-- import qualified Data.ByteString.Short as SBS
-- import System.Directory (createDirectoryIfMissing)
-- import System.FilePath (takeDirectory)
-- import Prelude (IO, FilePath, putStrLn, Either, ($), return)

-- -- Simple validator that always succeeds
-- {-# INLINABLE mkValidator #-}
-- mkValidator :: () -> () -> ScriptContext -> Bool
-- mkValidator _ _ _ = True

-- -- Compile the validator
-- validator :: Validator
-- validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

-- -- Function to write the compiled script to a file
-- writeValidator :: FilePath -> Validator -> IO (Either (FileError ()) ())
-- writeValidator file v = do
--     createDirectoryIfMissing True (takeDirectory file)
--     let script = PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise $ unValidatorScript v
--     writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing script

-- -- Write the script to a file
-- writeSimpleSuccessScript :: IO (Either (FileError ()) ())
-- writeSimpleSuccessScript = writeValidator "output/simple-success.plutus" validator

-- -- Main function
-- main :: IO ()
-- main = do
--     result <- writeSimpleSuccessScript
--     case result of
--         Left err -> print err
--         Right _  -> putStrLn "Simple Success script written successfully!"
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE TypeApplications #-}

-- module SmC.Compiler where

-- import PlutusTx.Prelude
-- import Plutus.V2.Ledger.Api
-- import Ledger (mkValidatorScript, unValidatorScript)
-- import qualified PlutusTx
-- import Cardano.Api (PlutusScript (..), writeFileTextEnvelope)
-- import Codec.Serialise (serialise)
-- import qualified Data.ByteString.Lazy as LBS
-- import qualified Data.ByteString.Short as SBS
-- import System.Directory (createDirectoryIfMissing)
-- import System.FilePath (takeDirectory)
-- import Prelude (IO, FilePath, putStrLn, Either, ($), return)

-- -- Simple validator that always succeeds
-- {-# INLINABLE mkValidator #-}
-- mkValidator :: () -> () -> ScriptContext -> Bool
-- mkValidator _ _ _ = True

-- -- Compile the validator
-- validator :: Validator
-- validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

-- -- Function to write the compiled script to a file
-- writeValidator :: FilePath -> Validator -> IO (Either (FileError ()) ())
-- writeValidator file v = do
--     createDirectoryIfMissing True (takeDirectory file)
--     let script = PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise $ unValidatorScript v
--     writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing script

-- -- Write the script to a file
-- writeSimpleSuccessScript :: IO (Either (FileError ()) ())
-- writeSimpleSuccessScript = writeValidator "output/simple-success.plutus" validator

-- -- Main function
-- main :: IO ()
-- main = do
--     result <- writeSimpleSuccessScript
--     case result of
--         Left err -> print err
--         Right _  -> putStrLn "Simple Success script written successfully!"
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- -- {-# LANGUAGE ScriptContext #-}
-- {-# LANGUAGE TypeApplications #-}

-- module SmC.Compiler where

-- import Plutus.V1.Ledger.Value (Value)
-- import Plutus.V2.Ledger.Api
-- import PlutusTx.Prelude

-- {-# INLINABLE rewardIfGreaterThan1000 #-}
-- rewardIfGreaterThan1000 :: Integer -> ScriptContext -> Bool
-- rewardIfGreaterThan1000 orderAmount ctx =
--     if orderAmount > 1000
--     then traceIfFalse "Order amount must be greater than 1000" True
--     else True

-- {-# INLINABLE mkValidatorScript #-}
-- mkValidatorScript :: Integer -> ScriptContext -> Bool
-- mkValidatorScript orderAmount ctx = rewardIfGreaterThan1000 orderAmount ctx

-- script :: Validator
-- script = mkValidatorScript

-- validator :: ValidatorHash
-- validator = ValidatorHash $ hashScript script

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module SmC.Compiler (writeFoodRewardScript) where

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
import Prelude (Show)
import Plutus.V2.Ledger.Api (ScriptContext)

import Plutus.V2.Ledger.Api (ScriptContext)

-- Define a data type to hold the order amount
data OrderAmount = OrderAmount { orderValue :: Integer } deriving (Show)

-- Function to check if order qualifies for reward
isRewardingOrder :: OrderAmount -> Bool
isRewardingOrder (OrderAmount orderValue) = orderValue > 1000

-- Define the script validator with a custom ScriptContext type
data FoodRewardContext = FoodRewardContext { orderAmount :: OrderAmount }

mkFoodRewardValidator :: ScriptContext -> Bool
mkFoodRewardValidator ctx = case txInfoValidRange (scriptContextTxInfo ctx) of
  Nothing -> traceIfFalse "Missing transaction info" False  -- Handle missing info
  Just interval ->
    case txInfoScriptContext ctx of
      Nothing -> traceIfFalse "Missing script context" False  -- Handle missing context
      Just (FoodRewardContext orderData) ->
        if isRewardingOrder orderData
        then traceIfFalse "Order amount must be greater than 1000" True
        else True
      _ -> traceIfFalse "Invalid script context type" False  -- Handle unexpected context type

-- Compile and write the script to a file
writeFoodRewardScript :: FilePath -> IO (Either (FileError ()) ())
writeFoodRewardScript :: FilePath -> IO (Either (FileError ()) ())
writeFoodRewardScript :: FilePath -> IO (Either (FileError ()) ())
writeFoodRewardScript file = do
  validator <- compile $ mkFoodRewardValidator
  writeValidator file validator
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