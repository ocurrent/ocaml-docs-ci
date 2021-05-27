#!/bin/bash

KEYFILE=${KEYFILE:-/ssh/id_ed25519}
KEYTYPE=${KEYTYPE:-ed25519}

if [ ! -f $KEYFILE ]; then
    echo "Creating a new keypair in $KEYFILE"
    ssh-keygen -q -t $KEYTYPE -f $KEYFILE -N '' <<<y
    cp $KEYFILE.pub /ssh/authorized_keys
fi

sleep infinity