#!/usr/bin/env bash

source variables-private.sh
source getTxFunc.sh

WALLET=$1

NETWORK="--testnet-magic 1"
cardano-cli query tip $NETWORK
cardano-cli query protocol-parameters $NETWORK --out-file protocol.json

export ADDRESS=$(cat $WALLET/payment.addr)
export SKEY="$WALLET/payment.skey"

echo ""
echo "What is the recipient address?"
echo ""
read RECEIVE_ADDRESS

echo ""
echo "------------------------------------------------------------------"
echo "Select a UTxO with lovelace:"
echo "------------------------------------------------------------------"
echo ""
chooseWalletUTxO $WALLET
TX_IN=${SELECTED_UTXO}


echo "How many lovelace will you send?"
read LOVELACE_TO_SEND

cardano-cli transaction build \
--babbage-era \
$NETWORK \
--tx-in $TX_IN \
--tx-out $RECEIVE_ADDRESS+$LOVELACE_TO_SEND \
--change-address $ADDRESS \
--protocol-params-file protocol.json \
--out-file send-lovelace.draft

cardano-cli transaction sign \
--tx-body-file send-lovelace.draft \
$NETWORK \
--signing-key-file $SKEY \
--out-file send-lovelace.signed

cardano-cli transaction submit \
$NETWORK \
--tx-file send-lovelace.signed
