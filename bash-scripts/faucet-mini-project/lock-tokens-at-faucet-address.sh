#!/usr/bin/env bash

# Fund a Token Gated Faucet on Preprod
source ../getTxFunc.sh
. ../variables-private.sh

rm build-tx.sh
touch build-tx.sh

echo "Enter path to CLI Wallet directory"
read WALLET
echo ""

ADDRESS=$(cat $WALLET/payment.addr)
SKEY=$WALLET/payment.skey

echo "Enter path to plutus script:"
read PATH_TO_FAUCET_SCRIPT
echo ""

cardano-cli address build --payment-script-file $PATH_TO_FAUCET_SCRIPT --testnet-magic 1 --out-file faucet.addr
FAUCET_ADDRESS=$(cat faucet.addr)

echo "Enter path to datum file:"
read PATH_TO_DATUM_FILE
echo ""

echo "cardano-cli transaction build \\" > build-tx.sh
echo "--babbage-era \\" >> build-tx.sh
echo "--testnet-magic 1 \\" >> build-tx.sh

echo ""
echo "--------------------------------------------------------------------------------------------"
echo "Choose Fees UTxO:"
chooseWalletUTxO $WALLET
TX_IN=${SELECTED_UTXO}
LOVELACE_IN=${SELECTED_UTXO_LOVELACE}
echo "--tx-in $TX_IN \\" >> build-tx.sh
echo ""
echo "--------------------------------------------------------------------------------------------"
echo "Choose UTxO with Faucet Tokens:"
chooseWalletUTxO $WALLET
TX_IN_TOKEN=${SELECTED_UTXO}
TOKENS_IN=${SELECTED_UTXO_TOKENS}
FAUCET_ASSET=${SELECTED_UTXO_ASSET}
echo "--tx-in $TX_IN_TOKEN \\" >> build-tx.sh
echo ""
echo "--------------------------------------------------------------------------------------------"

echo "How many tokens do you want to send to the faucet?"
read TOKENS_TO_FAUCET

TOKENS_BACK_TO_ISSUER=$(expr $TOKENS_IN - $TOKENS_TO_FAUCET)

echo "--tx-out $FAUCET_ADDRESS+\"2000000 + $TOKENS_TO_FAUCET $FAUCET_ASSET\" \\" >> build-tx.sh
echo "--tx-out-inline-datum-file $PATH_TO_DATUM_FILE \\" >> build-tx.sh
echo "--tx-out $ADDRESS+\"2000000 + $TOKENS_BACK_TO_ISSUER $FAUCET_ASSET\" \\" >> build-tx.sh
echo "--change-address $ADDRESS \\" >> build-tx.sh
echo "--protocol-params-file protocol.json \\" >> build-tx.sh
echo "--out-file open-a-faucet-tx.draft" >> build-tx.sh

cat build-tx.sh
. build-tx.sh

cardano-cli transaction sign \
--tx-body-file open-a-faucet-tx.draft \
--testnet-magic 1 \
--signing-key-file $SKEY \
--out-file open-a-faucet-tx.signed

cardano-cli transaction submit \
--testnet-magic 1 \
--tx-file open-a-faucet-tx.signed