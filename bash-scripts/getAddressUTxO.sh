#!/bin/bash

source variables-private.sh

cardano-cli query utxo --testnet-magic 1 --address $1