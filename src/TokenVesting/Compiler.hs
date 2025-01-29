{-# LANGUAGE DataKinds #-} 
{-# LANGUAGE DeriveAnyClass #-} 
{-# LANGUAGE DeriveGeneric #-} 
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE RecordWildCards #-} 
{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE TemplateHaskell #-} 
{-# LANGUAGE TypeApplications #-} 
{-# LANGUAGE TypeFamilies #-} 

module TokenVesting.Compiler where

-- import Cardano.Api 
-- import Codec.Serialise (serialise)
-- import Control.Monad (void)
-- import qualified Data.ByteString.Lazy as LBS
-- import qualified Data.ByteString.Short as SBS
-- import GHC.Generics (Generic)
-- import Plutus.V2.Ledger.Api
-- import Plutus.V1.Ledger.Crypto (PubKeyHash (..))
-- import Plutus.V1.Ledger.Interval (contains)
-- import qualified PlutusTx
-- import PlutusTx.Prelude
-- import Prelude (FilePath, IO, print, show)
-- import System.Directory (createDirectoryIfMissing)
-- import System.FilePath (takeDirectory)

-- import Cardano.Api
-- import Codec.Serialise (serialise)
-- import Control.Monad (void)
-- import qualified Data.ByteString.Lazy as LBS
-- import qualified Data.ByteString.Short as SBS
-- import GHC.Generics (Generic)
-- import Ledger (POSIXTime (..), Validator)
-- import qualified Ledger.Typed.Scripts as Scripts
-- import Plutus.V2.Ledger.Api
-- import Plutus.V1.Ledger.Interval (contains)
-- import Plutus.V1.Ledger.Crypto (PubKeyHash (..))
-- import qualified PlutusTx
-- import PlutusTx.Prelude
-- import Prelude (FilePath, IO, print, show)
-- import System.Directory (createDirectoryIfMissing)
-- import System.FilePath (takeDirectory)

import Cardano.Api 
import Codec.Serialise (serialise) 
import Control.Monad (void) 
import qualified Data.ByteString.Lazy as LBS 
import qualified Data.ByteString.Short as SBS 
import GHC.Generics (Generic) 
import Ledger (POSIXTime (..), Validator) 
import qualified Ledger.Typed.Scripts as Scripts 
import Plutus.V2.Ledger.Api 
import Plutus.V1.Ledger.Crypto (PubKeyHash (..)) 
import Plutus.V1.Ledger.Interval (contains) 
import qualified PlutusTx 
import PlutusTx.Prelude 
import Prelude (FilePath, IO, print, show) 
import System.Directory (createDirectoryIfMissing) 
import System.FilePath (takeDirectory)

-- Vesting Contract Datum
data VestingDatum = VestingDatum 
  { beneficiary :: PubKeyHash  -- Now imported!
  , vestingPeriodStart :: POSIXTime
  , vestingPeriodEnd :: POSIXTime
  , totalAmount :: Integer
  } deriving (Generic)

-- Redeemer to control withdrawal
data VestingAction = Withdraw | Terminate
    deriving (Generic)

-- Compile datum and redeemer types for on-chain use
PlutusTx.makeIsDataIndexed ''VestingDatum [('VestingDatum, 0)]
PlutusTx.makeIsDataIndexed ''VestingAction [('Withdraw, 0), ('Terminate, 1)]

{-# INLINABLE mkVestingValidator #-}
mkVestingValidator :: VestingDatum -> VestingAction -> ScriptContext -> Bool
mkVestingValidator VestingDatum{..} action ctx =
    case action of
        Withdraw ->
            traceIfFalse "Withdrawal before vesting period" vestingTimeValid &&
            traceIfFalse "Not signed by beneficiary" signedByBeneficiary
        Terminate ->
            traceIfFalse "Only beneficiary can terminate" signedByBeneficiary

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Check if current time is within vesting period
    vestingTimeValid :: Bool
    vestingTimeValid = 
        txInfoValidRange info `contains` 
        interval vestingPeriodStart vestingPeriodEnd

    -- Verify transaction is signed by beneficiary
    signedByBeneficiary :: Bool
    signedByBeneficiary = 
        txSignedBy info (unPaymentPubKeyHash beneficiary)

-- Compile the validator
vestingValidator :: Scripts.TypedValidator VestingType
vestingValidator = Scripts.mkTypedValidator @VestingType
    ($$(PlutusTx.compile [|| mkVestingValidator ||]))
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @VestingDatum @VestingAction

-- Define a custom script context type
data VestingType

-- Compilation function for writing the script
writeVestingScript :: IO (Either (FileError ()) ())
writeVestingScript = do
    let validator = Scripts.validatorScript vestingValidator
        scriptSBS = SBS.toShort $ LBS.toStrict $ serialise validator
        scriptSerial = PlutusScriptSerialised scriptSBS
    
    createDirectoryIfMissing True "output"
    
    writeFileTextEnvelope @(PlutusScript PlutusScriptV2) 
        "output/token-vesting.plutus" 
        Nothing 
        scriptSerial

-- Example usage function
exampleVestingSetup :: VestingDatum
exampleVestingSetup = VestingDatum
    { beneficiary = PaymentPubKeyHash "your-pubkey-hash-here"
    , vestingPeriodStart = POSIXTime 1640995200000  -- Jan 1, 2022
    , vestingPeriodEnd = POSIXTime 1672531200000    -- Jan 1, 2023
    , totalAmount = 100000000  -- 100 tokens
    }

-- Main function for demonstration
main :: IO ()
main = do
    result <- writeVestingScript
    case result of
        Left err -> print err
        Right _ -> putStrLn "Vesting script written successfully!"
