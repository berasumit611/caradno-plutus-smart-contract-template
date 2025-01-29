#!/usr/bin/env bash

# Fund a Token Gated Faucet on Preprod
source ../getTxFunc.sh
. ../variables-private.sh

rm build-tx.sh
touch build-tx.sh

# Instead of prompting for input in the terminal (which can get annoying during a testing phase!),
# you can add the file paths to Datum and Redeemer files here.
DATUM_FILE="faucet-datum.json"
REDEEMER_FILE="faucet-redeemer.json"

echo "Enter path to CLI Wallet directory"
read WALLET
echo ""

ADDRESS=$(cat $WALLET/payment.addr)
SKEY=$WALLET/payment.skey

echo "Enter path to plutus script:"
read PATH_TO_FAUCET_SCRIPT
echo ""

echo "Enter plutus reference UTxO (TxHash#TxIx):"
read SCRIPT_REFERENCE_UTXO
echo ""

cardano-cli address build --payment-script-file $PATH_TO_FAUCET_SCRIPT --testnet-magic 1 --out-file faucet.addr
FAUCET_ADDRESS=$(cat faucet.addr)

echo "cardano-cli transaction build \\" > build-tx.sh
echo "--babbage-era \\" >> build-tx.sh
echo "--testnet-magic 1 \\" >> build-tx.sh

echo ""
echo "--------------------------------------------------------------------------------------------"
echo "Choose Fees/Collateral UTxO:"
chooseWalletUTxO $WALLET
TX_IN=${SELECTED_UTXO}
LOVELACE_IN=${SELECTED_UTXO_LOVELACE}
echo "--tx-in $TX_IN \\" >> build-tx.sh
echo "--tx-in-collateral $TX_IN \\" >> build-tx.sh
echo ""
echo "--------------------------------------------------------------------------------------------"
echo "Choose UTxO with PPBL 2023 Token:"
chooseWalletUTxO $WALLET
TX_IN_ACCESS_TOKEN=${SELECTED_UTXO}
TOKENS_IN=${SELECTED_UTXO_TOKENS}
ACCESS_ASSET=${SELECTED_UTXO_ASSET}
echo "--tx-in $TX_IN_ACCESS_TOKEN \\" >> build-tx.sh
echo ""
echo "--------------------------------------------------------------------------------------------"
echo "Choose Faucet UTxO:"
chooseContractUTxO $FAUCET_ADDRESS
TX_IN_FROM_FAUCET=${SELECTED_UTXO}
FAUCET_TOKENS_IN=${SELECTED_UTXO_TOKENS}
FAUCET_ASSET=${SELECTED_UTXO_ASSET}
echo "--tx-in $TX_IN_FROM_FAUCET \\" >> build-tx.sh
echo "--spending-tx-in-reference $SCRIPT_REFERENCE_UTXO \\" >> build-tx.sh
echo "--spending-plutus-script-v2 \\" >> build-tx.sh
echo "--spending-reference-tx-in-inline-datum-present \\" >> build-tx.sh
echo "--spending-reference-tx-in-redeemer-file $REDEEMER_FILE \\" >> build-tx.sh
echo ""
echo "--------------------------------------------------------------------------------------------"

echo "How many tokens do you want to witdraw from the faucet?"
read TOKENS_FROM_FAUCET

TOKENS_TO_FAUCET=$(expr $FAUCET_TOKENS_IN - $TOKENS_FROM_FAUCET)

echo "--tx-out $FAUCET_ADDRESS+\"2000000 + $TOKENS_TO_FAUCET $FAUCET_ASSET\" \\" >> build-tx.sh
echo "--tx-out-inline-datum-file $DATUM_FILE \\" >> build-tx.sh
echo "--tx-out $ADDRESS+\"2000000 + $TOKENS_FROM_FAUCET $FAUCET_ASSET\" \\" >> build-tx.sh
echo "--tx-out $ADDRESS+\"2000000 + 1 $ACCESS_ASSET\" \\" >> build-tx.sh
echo "--change-address $ADDRESS \\" >> build-tx.sh
echo "--protocol-params-file protocol.json \\" >> build-tx.sh
echo "--out-file test-a-faucet.draft" >> build-tx.sh

cat build-tx.sh
. build-tx.sh

cardano-cli transaction sign \
--tx-body-file test-a-faucet.draft \
--testnet-magic 1 \
--signing-key-file $SKEY \
--out-file test-a-faucet.signed

cardano-cli transaction submit \
--testnet-magic 1 \
--tx-file test-a-faucet.signed