#!/usr/bin/env bash

if [ "$#" -ne 1 ]; then
  echo "Usage: mint-ppblnft (path-to-wallet-directory)"
  return
fi

source getTxFunc.sh
source variables-private.sh

cardano-cli query protocol-parameters --testnet-magic 1 --out-file protocol.json

WALLET=$1

ADDRESS=$(cat $WALLET/payment.addr)
SKEY=$WALLET/payment.skey

POLICY_ID="2a384dc205a97463577fc98b704b537f680c0eba84126eb7d5857c86"
REF_UTXO="43cf9133e5ba3e2a79a81b048137eab9eda574b889cd0d5e2a0b5ca329cdbf45#0"

echo ""
echo "*******************************************************************************************"
echo ""
echo "Plutus PBL 2023"
echo "Mastery Assignment 203.1"
echo ""
echo "This script will guide you through the process of minting a PPBL NFT on Preprod."
echo "Before you use this script, please complete the following steps:"
echo ""
echo "  1. Send your PPBL 2023 Token to your CLI Wallet."
echo "  2. Prepare a ppbl-nft.json file as described in Lesson 203.2 and Assignment 203.1"
echo ""
echo ""
echo "This uses a reference UTxO for the smart contract."
echo ""
echo ""
echo "When you are ready, press any key to continue."
echo ""
echo "*******************************************************************************************"
echo ""
read -n 1 -s
echo ""
echo "You will mint a token from the address:"
echo $ADDRESS
echo ""
echo "Press any key to continue."
echo ""
read -n 1 -s

echo "Enter Preprod address to receive tokens:"
read RECEIVER
echo ""

echo "Enter the exact name of your token (starting with PPBL2023):"
read TOKEN_NAME
echo ""
TOKEN_HEXSTRING=$(xxd -pu <<<$TOKEN_NAME)
TOKEN_HEX=${TOKEN_HEXSTRING::-2}

echo "How many tokens do you want to mint?"
read QUANTITY
echo ""
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
echo "Choose a UTxO to pay transaction fees:"
echo ""
chooseWalletUTxO $WALLET
TX_IN=${SELECTED_UTXO}

echo "Choose the UTxO with your PPBL 2023 Token:"
echo ""
chooseWalletUTxO $WALLET
CONTRIB_TX_IN=${SELECTED_UTXO}
CONTRIB_ASSET=${SELECTED_UTXO_ASSET}

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
  --tx-in $CONTRIB_TX_IN \
  --tx-in-collateral $TX_IN \
  --tx-out $RECEIVER+"1500000 + $QUANTITY $POLICY_ID.$TOKEN_HEX" \
  --tx-out $RECEIVER+"1500000 + 1 $CONTRIB_ASSET" \
  --mint "$QUANTITY $POLICY_ID.$TOKEN_HEX" \
  --mint-tx-in-reference $REF_UTXO \
  --policy-id $POLICY_ID \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-value \"$TOKEN_NAME\" \
  --change-address $ADDRESS \
  --metadata-json-file ./metadata/ppbl-nft.json \
  --protocol-params-file protocol.json \
  --out-file mint-native-assets-with-plutus.draft

cardano-cli transaction sign \
  --signing-key-file $SKEY \
  --testnet-magic 1 \
  --tx-body-file mint-native-assets-with-plutus.draft \
  --out-file mint-native-assets-with-plutus.signed

cardano-cli transaction submit \
  --tx-file mint-native-assets-with-plutus.signed \
  --testnet-magic 1
