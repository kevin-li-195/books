#!/bin/bash

ssh jerrington.me bash <<EOF
sudo su-renewal
cd books
git pull
make
EOF
