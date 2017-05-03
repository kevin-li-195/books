#!/bin/bash

git push

ssh jerrington.me bash <<EOF
sudo su-renewal
cd books
git fetch
git reset --hard origin/master
make $1
EOF
