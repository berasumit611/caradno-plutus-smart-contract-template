#!/bin/bash

cardano-node run \
--topology <YOUR PATH TO>/config-preprod/topology.json \
--database-path <YOUR PATH TO>/cardano/pre-production/db \
--socket-path <YOUR PATH TO>/cardano/pre-production/db/node.socket \
--host-addr 0.0.0.0 \
--port 3001 \
--config <YOUR PATH TO>/config-preprod/config.json