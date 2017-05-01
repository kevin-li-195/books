#!/bin/bash

make frontend
(cd deploy ; exec python -m http.server 8888 )
