-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE TypeApplications #-}

-- module FoodOrderReward.Compiler 
--     ( writeFoodOrderRewardScript
--     , writeTypedFoodOrderRewardScript
--     ) where

-- import Cardano.Api
-- import Codec.Serialise (serialise)
-- import qualified Data.ByteString.Lazy as LBS
-- import qualified Data.ByteString.Short as SBS
-- import qualified Plutus.V2.Ledger.Api
-- import Cardano.Api.Shelley (PlutusScript(..))
-- import System.Directory (createDirectoryIfMissing)
-- import System.FilePath (takeDirectory)
-- import qualified PlutusTx
-- import PlutusTx.Prelude
-- import Prelude (FilePath, IO, putStrLn, show, print)

-- -- Validator logic for food order rewards
-- {-# INLINABLE mkValidator #-}
-- mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
-- mkValidator datum redeemer ctx =
--     let orderAmount = PlutusTx.unsafeFromBuiltinData datum
--     in if orderAmount > 1000
--        then check True
--        else check False
--   where 
--     check :: Bool -> ()
--     check b = check' b
    
--     check' :: Bool -> ()
--     check' True = ()
--     check' False = traceError "Order amount is too low for a reward"

-- -- Compile the validator
-- {-# INLINABLE mkValidatorCode #-}
-- mkValidatorCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
-- mkValidatorCode = $$(PlutusTx.compile [|| mkValidator ||])

-- -- Create validator script
-- validator :: Plutus.V2.Ledger.Api.Validator
-- validator = Plutus.V2.Ledger.Api.mkValidatorScript mkValidatorCode

-- -- General function to write a compiled Plutus script to a file
-- writeValidator :: FilePath -> Plutus.V2.Ledger.Api.Validator -> IO (Either (FileError ()) ())
-- writeValidator file val = do
--     createDirectoryIfMissing True (takeDirectory file)
--     result <- writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing $
--         PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise $ Plutus.V2.Ledger.Api.unValidatorScript val
--     case result of
--         Left err -> putStrLn $ "Failed to write script: " <> show err
--         Right () -> putStrLn $ "Successfully wrote script to: " <> file
--     return result

-- -- Writes the untyped "Food Order Reward" script to a file
-- writeFoodOrderRewardScript :: IO (Either (FileError ()) ())
-- writeFoodOrderRewardScript = writeValidator "output/food-order-reward.plutus" validator

-- -- Optional: Typed version of script writing (if needed)
-- writeTypedFoodOrderRewardScript :: IO (Either (FileError ()) ())
-- writeTypedFoodOrderRewardScript = writeValidator "output/typed-food-order-reward.plutus" validator

-- -- Main function to demonstrate script writing
-- main :: IO ()
-- main = do
--     result <- writeFoodOrderRewardScript
--     case result of
--         Left err -> print err
--         Right _  -> putStrLn "Food Order Reward script written successfully!"
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE TypeApplications #-}

-- module FoodOrderReward.Compiler where

-- import Cardano.Api
-- import Codec.Serialise (serialise)
-- import qualified Data.ByteString.Lazy as LBS
-- import qualified Data.ByteString.Short as SBS
-- import qualified Plutus.V2.Ledger.Api
-- import Cardano.Api.Shelley (PlutusScript(..))
-- import System.Directory (createDirectoryIfMissing)
-- import System.FilePath (takeDirectory)
-- import qualified PlutusTx
-- import PlutusTx.Prelude
-- import Prelude (FilePath, IO, putStrLn, show, print)

-- -- Simple addition validator
-- {-# INLINABLE mkAddValidator #-}
-- mkAddValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
-- mkAddValidator datum redeemer ctx =
--     let 
--         -- Decode the inputs
--         inputs = PlutusTx.unsafeFromBuiltinData datum
        
--         -- Pattern match on the input tuple
--         (a, b) = inputs
        
--         -- Expected sum (passed as redeemer)
--         expectedSum = PlutusTx.unsafeFromBuiltinData redeemer
--     in 
--         -- Check if the actual sum matches the expected sum
--         check (a + b == expectedSum)
--   where 
--     check :: Bool -> ()
--     check True = ()
--     check False = traceError "Addition validation failed: Sum does not match"

-- -- Compile the validator
-- {-# INLINABLE mkAddValidatorCode #-}
-- mkAddValidatorCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
-- mkAddValidatorCode = $$(PlutusTx.compile [|| mkAddValidator ||])

-- -- Create validator script
-- validator :: Plutus.V2.Ledger.Api.Validator
-- validator = Plutus.V2.Ledger.Api.mkValidatorScript mkAddValidatorCode

-- -- Function to write the compiled script to a file
-- writeValidator :: FilePath -> Plutus.V2.Ledger.Api.Validator -> IO (Either (FileError ()) ())
-- writeValidator file val = do
--     createDirectoryIfMissing True (takeDirectory file)
--     result <- writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing $
--         PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise $ Plutus.V2.Ledger.Api.unValidatorScript val
--     case result of
--         Left err -> putStrLn $ "Failed to write script: " <> show err
--         Right () -> putStrLn $ "Successfully wrote script to: " <> file
--     return result

-- -- Write the addition script to a file
-- writeAddScript :: IO (Either (FileError ()) ())
-- writeAddScript = writeValidator "output/add-script.plutus" validator

-- -- Main function to demonstrate script writing
-- main :: IO ()
-- main = do
--     result <- writeAddScript
--     case result of
--         Left err -> print err
--         Right _  -> putStrLn "Addition script written successfully!"
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module FoodOrderReward.Compiler where

import Cardano.Api
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Plutus.V2.Ledger.Api
import Cardano.Api.Shelley (PlutusScript(..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import qualified PlutusTx
import PlutusTx.Prelude
import Prelude (FilePath, IO, putStrLn, show, print)

-- Hello World Validator
{-# INLINABLE mkHelloWorldValidator #-}
mkHelloWorldValidator :: BuiltinData -> BuiltinData -> BuiltinData -> Bool
mkHelloWorldValidator datum redeemer ctx =
    let 
        -- Decode the input message
        message = PlutusTx.unsafeFromBuiltinData datum
        
        -- Check if the message is "Hello, World!"
        expectedMessage = "Hello, World!" :: BuiltinString
    in 
        -- Validate the message
        check (message == expectedMessage)
  where 
    check :: Bool -> ()
    check True = traceIfFalse "Hello World Verified!" ()
    check False = traceError "Invalid Hello World message"

-- Compile the validator
{-# INLINABLE mkHelloWorldValidatorCode #-}
mkHelloWorldValidatorCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
mkHelloWorldValidatorCode = $$(PlutusTx.compile [|| mkHelloWorldValidator ||])

-- Create validator script
validator :: Plutus.V2.Ledger.Api.Validator
validator = Plutus.V2.Ledger.Api.mkValidatorScript mkHelloWorldValidatorCode

-- Function to write the compiled script to a file
writeValidator :: FilePath -> Plutus.V2.Ledger.Api.Validator -> IO (Either (FileError ()) ())
writeValidator file val = do
    createDirectoryIfMissing True (takeDirectory file)
    result <- writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing $
        PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise $ Plutus.V2.Ledger.Api.unValidatorScript val
    case result of
        Left err -> putStrLn $ "Failed to write script: " <> show err
        Right () -> putStrLn $ "Successfully wrote script to: " <> file
    return result

-- Write the Hello World script to a file
writeHelloWorldScript :: IO (Either (FileError ()) ())
writeHelloWorldScript = writeValidator "output/hello-world-script.plutus" validator

-- Main function to demonstrate script writing
main :: IO ()
main = do
    result <- writeHelloWorldScript
    case result of
        Left err -> print err
        Right _  -> putStrLn "Hello World script written successfully!"