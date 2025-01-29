{-# LANGUAGE OverloadedStrings #-}

module GenerateDatum where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Aeson (encode, Value(..), object, (.=))
import System.IO (writeFile)

-- | Example Datum (replace with your own logic)
-- This script generates a datum with a simple integer value
datumValue :: Integer
datumValue = 42 -- Replace this with your actual datum value

-- | Write the datum to a JSON file
writeDatumJSON :: FilePath -> IO ()
writeDatumJSON filePath = do
    let datum = Number (fromIntegral datumValue)  -- Create a JSON value
        encodedDatum = encode datum               -- Convert to ByteString
    LBS.writeFile filePath encodedDatum           -- Write to file
    putStrLn $ "Datum JSON saved to: " ++ filePath

main :: IO ()
main = writeDatumJSON "datum.json"
