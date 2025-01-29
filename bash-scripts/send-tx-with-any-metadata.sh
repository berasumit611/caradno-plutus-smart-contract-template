#!/usr/bin/env bash

if [ "$#" -ne 1 ]; then
  echo "Usage: . send-tx-with-any-metadata.sh (path-to-wallet-directory)"
  return
fi

source getTxFunc.sh
source variables-private.sh

cardano-cli query protocol-parameters --testnet-magic 1 --out-file protocol.json

WALLET=$1

ADDRESS=$(cat $WALLET/payment.addr)
SKEY=$WALLET/payment.skey

echo ""
echo "*******************************************************************************************"
echo ""
echo "Plutus PBL 2023"
echo "Lesson 203.1"
echo ""
echo "This script will help you build, sign, and submit a Cardano transaction with metadata."
echo ""
echo "Before you get started, make sure to create a message.json file,"
echo "as described in Lesson 203.1."
echo ""
echo "Press any key to continue."
echo ""
echo "*******************************************************************************************"
echo ""
read -n 1 -s
echo ""

echo "Enter path to metadata file:"
read METADATA_FILE_PATH

echo "Found message.json, with contents: "
cat $METADATA_FILE_PATH
echo ""
echo ""
echo "You will send a transaction back to your address:"
echo $ADDRESS
echo ""
echo "Press any key to continue."
read -n 1 -s
echo ""

echo "Choose a UTxO to pay transaction fees:"
echo ""
chooseWalletUTxO $WALLET
TX_IN=${SELECTED_UTXO}

echo ""
echo "*******************************************************************************************"
echo ""
echo "Building transaction..."
echo ""
echo "*******************************************************************************************"
echo ""
sleep 1

cardano-cli transaction build \
--babbage-era \
--testnet-magic 1 \
--tx-in $TX_IN \
--change-address $ADDRESS \
--metadata-json-file $METADATA_FILE_PATH \
--protocol-params-file protocol.json \
--out-file tx-with-metadata.draft

echo ""
echo "*******************************************************************************************"
echo ""
echo "Signing transaction..."
echo ""
echo "*******************************************************************************************"
echo ""
sleep 1

cardano-cli transaction sign \
--signing-key-file $SKEY \
--testnet-magic 1 \
--tx-body-file tx-with-metadata.draft \
--out-file tx-with-metadata.signed

echo ""
echo "*******************************************************************************************"
echo ""
echo "Submitting transaction..."
echo ""
echo "*******************************************************************************************"
echo ""
sleep 1

cardano-cli transaction submit \
--tx-file tx-with-metadata.signed \
--testnet-magic 1
