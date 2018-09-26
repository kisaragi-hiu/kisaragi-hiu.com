#!/bin/bash

if [ "$TRAVIS_PULL_REQUEST" != "false" ]; then
    echo "This is a pull request. Not deploying."
    exit 0
fi

cd ~/public || exit 1
git init

git config user.name "Built on Travis CI"
git config user.email "mail@kisaragi-hiu.com"

git add .
git commit -m "Deploy"

git push --force "https://${GH_TOKEN}@${GH_REF}" master
