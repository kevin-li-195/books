#!/bin/bash

ssh jerrington.me bash <<EOF
sudo journalctl -xfu books-renewal.service
EOF
