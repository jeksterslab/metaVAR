#!/bin/bash

git clone git@github.com:jeksterslab/metaVAR.git
rm -rf "$PWD.git"
mv metaVAR/.git "$PWD"
rm -rf metaVAR
