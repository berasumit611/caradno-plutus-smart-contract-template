source project-variables.sh

cardano-cli address build --payment-script-file $PATH_TO_PLUTUS_SCRIPT --testnet-magic 1 --out-file faucet.addr
FAUCET_ADDRESS=$(cat faucet.addr)

cardano-cli query utxo --testnet-magic 1 --address $FAUCET_ADDRESS