#!/bin/bash

# Make a copy of this file with the name variables-private.sh
# Then, set the CARDANO_NODE_SOCKET_PATH to match your local set-up

export CARDANO_NODE_SOCKET_PATH=<YOUR PATH TO>/db/node.socket

# Optionally, you can add your CLI address and private key to a file like this, for more efficient accces:
# export SENDERADDRESS=""
# export SENDERKEY=""