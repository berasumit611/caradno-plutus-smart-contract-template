#!/usr/bin/env bash

if [ "$#" -ne 1 ]; then
  echo "Usage: mint-tokens-native-script (path-to-wallet-directory)"
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
echo "Lesson 202.1"
echo ""
echo "This script will guide you through the process of minting a native asset on Cardano"
echo ""
echo "Before you get started, make sure to create a minting script file,"
echo "as described in Lesson 202.1."
echo ""
echo "Press any key to continue."
echo ""
echo "*******************************************************************************************"
echo ""
read -n 1 -s
echo ""
echo "You will mint a token from the address:"
echo $ADDRESS
echo ""
echo "Press any key to continue."
read -n 1 -s
echo ""
echo "Enter path to minting script:"
read MINT_SCRIPT_FILE_PATH
echo ""

POLICY_ID=$(cardano-cli transaction policyid --script-file $MINT_SCRIPT_FILE_PATH)

echo "Enter Preprod address to receive tokens:"
read RECEIVER
echo ""

echo "Enter the name of your token:"
read TOKEN_NAME
echo ""

echo "How many tokens do you want to mint?"
read QUANTITY
echo ""


TOKEN_HEXSTRING=$(xxd -pu <<< $TOKEN_NAME)
TOKEN_HEX=${TOKEN_HEXSTRING::-2}
echo "*******************************************************************************************"
echo ""
echo "Ok, you will mint $QUANTITY $TOKEN_NAME tokens with the Policy Id"
echo $POLICY_ID
echo ""
echo "Press any key to continue."
echo ""
echo "*******************************************************************************************"
read -n 1 -s
echo ""
echo "Finally, choose a UTxO to pay transaction fees:"
echo ""
chooseWalletUTxO $WALLET
TX_IN=${SELECTED_UTXO}

echo ""
echo "*******************************************************************************************"
echo ""
echo "Minting!"
echo ""
echo "*******************************************************************************************"
echo ""

cardano-cli transaction build \
--babbage-era \
--testnet-magic 1 \
--tx-in $TX_IN \
--tx-out $RECEIVER+"1500000 + $QUANTITY $POLICY_ID.$TOKEN_HEX" \
--mint "$QUANTITY $POLICY_ID.$TOKEN_HEX" \
--mint-script-file $MINT_SCRIPT_FILE_PATH \
--change-address $ADDRESS \
--protocol-params-file protocol.json \
--out-file mint-native-assets.draft

cardano-cli transaction sign \
--signing-key-file $SKEY \
--testnet-magic 1 \
--tx-body-file mint-native-assets.draft \
--out-file mint-native-assets.signed

cardano-cli transaction submit \
--tx-file mint-native-assets.signed \
--testnet-magic 1