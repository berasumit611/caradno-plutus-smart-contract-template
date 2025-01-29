{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module MyHelloWorld.Compiler (writeHelloWorldScript, writeTypedHelloWorldScript) where

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


-- Import your new Hello World validators
import AlwaysSucceeds.Validator as HelloWorld
import AlwaysSucceeds.TypedValidator as TypedHelloWorld

-- General function to write a compiled Plutus script to a file
--  function writeValidator takes a file path and a Plutus validator script as arguments.
writeValidator :: FilePath -> Plutus.V2.Ledger.Api.Validator -> IO (Either (FileError ()) ())
writeValidator file validator = do
    createDirectoryIfMissing True (takeDirectory file)
    result <- writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing $
        PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise $ Plutus.V2.Ledger.Api.unValidatorScript validator
    case result of
        Left err -> putStrLn $ "Failed to write script: " <> show err
        Right () -> putStrLn $ "Successfully wrote script to: " <> file
    return result

-- Writes the untyped "Hello World" script to a file
writeHelloWorldScript :: IO (Either (FileError ()) ())
writeHelloWorldScript = writeValidator "output/hello-world.plutus" HelloWorld.validator

-- Writes the typed "Hello World" script to a file
writeTypedHelloWorldScript :: IO (Either (FileError ()) ())
writeTypedHelloWorldScript = writeValidator "output/typed-hello-world.plutus" TypedHelloWorld.validator
-- define two functions: writeHelloWorldScript and writeTypedHelloWorldScript.