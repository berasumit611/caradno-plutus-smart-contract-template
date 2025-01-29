-- SMART CONTRACT CONDITION

-- Plutus smart contract in Haskell where the validation logic ensures that the redeemer's user address matches the datum's user address. 
-- This logic is commonly used for conditions like restricting access to certain assets or actions to specific users.

-- Datum: This will hold the user’s address (or other identifying information).
-- Redeemer: This will also hold the user’s address attempting to redeem the asset.
-- Validation Logic: The validator will check if the address in the redeemer matches the address in the datum.

{-# LANGUAGE DataKinds#-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module AddressValidator.Contract where

import Plutus.V2.Ledger.Api      (BuiltinData, Validator, mkValidatorScript, ScriptContext, PubKeyHash, unValidatorScript)
import PlutusTx                 (compile, toBuiltinData, unsafeFromBuiltinData, unstableMakeIsData)
import PlutusTx.Prelude         (traceIfFalse, (==), error, Integer, Maybe(Nothing), (.), (&&), isJust, (==),Either(Left), Either(Right),(*),Bool(True))
import Prelude                  (IO, FilePath, writeFile, Show, Bool, Integer, print, (++), String, ($), putStrLn, undefined)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Codec.Serialise          (serialise)
import Cardano.Api
import Cardano.Api.Shelley      (PlutusScript (..), writeFileTextEnvelope, displayError)
import System.Directory         (createDirectoryIfMissing)
import System.FilePath          (takeDirectory)


-- Datum structure to store user address
-- this is the custom data type of haskell language
data UserDatum = UserDatum {userAddress::PubKeyHash} deriving Show

PlutusTx.unstableMakeIsData ''UserDatum

-- Redeemer structure to store the user address attempting to redeem
-- this is the custom data type of haskell language
data UserRedeemer = UserRedeemer{redeemerAddress::PubKeyHash} deriving Show

PlutusTx.unstableMakeIsData ''UserRedeemer

-- Validator logic: Checks if the redeemer's address matches the datum's address
{-# INLINABLE validateAddress #-}
validateAddress:: UserDatum -> UserRedeemer -> ScriptContext -> ()
validateAddress datum redeemer _ =
    let 
        -- Compare the address in the datum with the one in the redeemer
        isValid = userAddress datum == redeemerAddress redeemer
    in
        if traceIfFalse "Address mismatch!!" isValid
            then ()
            else error () -- Fails the transaction if the address don't match


{-# INLINABLE wrapValidator #-}
wrapValidator::BuiltinData -> BuiltinData-> BuiltinData -> ()
wrapValidator datum redeemer context =
    validateAddress 
        (unsafeFromBuiltinData datum)
        (unsafeFromBuiltinData redeemer)
        (unsafeFromBuiltinData context)

-- Compile the validator into a Plutus script
validator::Validator
validator=mkValidatorScript $$(compile [|| wrapValidator ||])

-- Write the compiled script to a file for deployment
writePlutusScript :: FilePath -> Validator -> IO()
writePlutusScript file validator = do
    createDirectoryIfMissing True (takeDirectory file)
    let serialised = PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise $ unValidatorScript validator
    result <- writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing serialised
    case result of
        Left err -> putStrLn $ displayError err
        Right () -> putStrLn $ "Script written to: " ++ file

-- | Main entry point to write the script.
writeScript :: IO ()
writeScript = writePlutusScript "AddressValidator/contract.plutus" validator