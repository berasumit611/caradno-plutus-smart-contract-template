
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module MathGame.Game where

import Plutus.V2.Ledger.Api      (BuiltinData, Validator, mkValidatorScript, ScriptContext, unValidatorScript, Datum(..), Redeemer(..), scriptContextTxInfo, txInfoInputs, TxOut(..))
import PlutusTx                 (compile, toBuiltinData, unsafeFromBuiltinData, BuiltinData, unstableMakeIsData)
import PlutusTx.Prelude         (traceIfFalse, (==), error, Integer, Maybe(Nothing), (.), (&&), isJust, (==),Either(Left), Either(Right),(*),Bool(True))
import Prelude                  (IO, FilePath, writeFile, Show, Bool, Integer, print, (++), String, ($), putStrLn, undefined)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Cardano.Api
import Codec.Serialise (serialise)
import Cardano.Api.Shelley (PlutusScript (..), writeFileTextEnvelope, displayError)
import qualified Plutus.V2.Ledger.Api

data RValue = RValue
    {
    targetGuess:: Integer
    }
    deriving Show
    -- deriving show
PlutusTx.unstableMakeIsData ''RValue

-- | Validator script logic: Validates if the guess matches the square root of the target.

{-# INLINABLE validateGuess #-}
validateGuess :: RValue -> BuiltinData -> BuiltinData -> ()
validateGuess datum redeemer _ =
    let
        -- The user's guess (redeemer)
        guess = unsafeFromBuiltinData @Integer redeemer
        -- Check if the guess squared equals the target value
    in
        if traceIfFalse "Incorrect guess!" (guess * guess == targetGuess datum)
            then ()
            else error ()  -- This ensures the function always returns ().


{-# INLINABLE wrapValidator #-}
wrapValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapValidator datum redeemer context =
    validateGuess
        (unsafeFromBuiltinData datum)
        (unsafeFromBuiltinData redeemer)
        context

-- | Compile the validator into a Plutus script.
validator :: Validator
validator = mkValidatorScript $$(compile [|| wrapValidator ||])

-- | Write the compiled script to a file for deployment.
writePlutusScript :: FilePath -> Validator -> IO ()
writePlutusScript file validator = do 
    createDirectoryIfMissing True (takeDirectory file)
    result <- writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing $
        PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise $ Plutus.V2.Ledger.Api.unValidatorScript validator
    case result of 
        Left err -> print $ displayError err 
        Right () -> putStrLn "Successfully wrote Plutus script to file."

writeScript :: IO ()
writeScript = writePlutusScript "mathGame/squareMathBounty.plutus" validator
