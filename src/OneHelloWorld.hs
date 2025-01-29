{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module OneHelloWorld where


import Plutus.V2.Ledger.Api      (BuiltinData, Validator, mkValidatorScript, ScriptContext, TxInfo, scriptContextTxInfo)
-- import Ledger.Address (PaymentPubKeyHash (PaymentPubKeyHash))
import Plutus.V2.Ledger.Api            (PaymentPubKeyHash)
-- import Ledger.Address                   (PaymentPubKeyHash(..))
import PlutusTx                 (compile, toBuiltinData, unsafeFromBuiltinData)
import PlutusTx.Prelude         (traceIfFalse, (==), error, Bool(True), Bool(False), ($))
import Prelude                  (IO, FilePath, putStrLn)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Codec.Serialise (serialise)
import Cardano.Api
import Cardano.Api.Shelley (PlutusScript (..), writeFileTextEnvelope, displayError)

-- | Validator script logic: Checks if assets can be unlocked.
{-# INLINABLE validateLockUnlock #-}
validateLockUnlock :: BuiltinData -> BuiltinData -> BuiltinData -> ScriptContext -> Bool
validateLockUnlock datum redeemer context =
    traceIfFalse "Invalid signer" checkSigner &&
    traceIfFalse "Invalid unlock message" checkMessage
  where
    -- Extract transaction info
    info :: TxInfo
    info = scriptContextTxInfo context

    -- The signer's public key hash is stored in the datum.
    signer :: PaymentPubKeyHash
    signer = unsafeFromBuiltinData datum

    -- Check that the transaction is signed by the locker's public key hash.
    checkSigner :: Bool
    checkSigner = txSignedBy info (unPaymentPubKeyHash signer)

    -- The redeemer must contain the message "Hello, World!".
    unlockMessage :: BuiltinData
    unlockMessage = unsafeFromBuiltinData redeemer

    checkMessage :: Bool
    checkMessage = unlockMessage == toBuiltinData ("Hello, World!" :: String)

-- | Compile the validator into a Plutus script.
validator :: Validator
validator = mkValidatorScript $$(compile [|| validateLockUnlock ||])

-- | Write the compiled script to a file for deployment.
writePlutusScript :: FilePath -> Validator -> IO ()
writePlutusScript file validator = do
    createDirectoryIfMissing True (takeDirectory file)
    let script = PlutusScriptSerialised . SBS.toShort . LBS.toStrict $ serialise validator
    result <- writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing script
    case result of
        Left err -> putStrLn $ "Error writing script: " ++ displayError err
        Right () -> putStrLn $ "Successfully wrote Plutus script to file: " ++ file

-- | Write the Hello World smart contract to a file.
writeScript :: IO ()
writeScript = writePlutusScript "OneHelloWorld/OneHelloWorld.plutus" validator
