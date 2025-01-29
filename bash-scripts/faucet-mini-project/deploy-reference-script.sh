#!/usr/bin/env bash

source getTxFunc.sh
source variables-private.sh

cardano-cli query protocol-parameters --testnet-magic 1 --out-file protocol.json

echo "Enter path to CLI Wallet directory"
read WALLET
echo ""

ADDRESS=$(cat $WALLET/payment.addr)
SKEY=$WALLET/payment.skey

echo "Enter path to Plutus Script"
read PLUTUS_SCRIPT
echo ""

OUT=$(cardano-cli transaction calculate-min-required-utxo --tx-out $ADDRESS+1000000 --tx-out-reference-script-file $PLUTUS_SCRIPT --protocol-params-file protocol.json)

MIN_UTXO=$(echo $OUT | awk '{ print $2 }')

echo "Ok, will deploy $PLUTUS_SCRIPT to $ADDRESS"
echo "With $MIN_UTXO lovelace"


echo "--------------------------------------------------------------------------------------------"
echo "Choose Lovelace UTxO:"
chooseWalletUTxO $WALLET
TX_IN=${SELECTED_UTXO}
echo ""

cardano-cli transaction build \
--babbage-era \
--testnet-magic 1 \
--tx-in $TX_IN \
--tx-out $ADDRESS+$MIN_UTXO \
--tx-out-reference-script-file $PLUTUS_SCRIPT \
--change-address $ADDRESS \
--protocol-params-file protocol.json \
--out-file create-reference-utxo.draft

cardano-cli transaction sign \
--tx-body-file create-reference-utxo.draft \
--testnet-magic 1 \
--signing-key-file $SKEY \
--out-file create-reference-utxo.signed

cardano-cli transaction submit \
--testnet-magic 1 \
--tx-file create-reference-utxo.signed

echo ""
