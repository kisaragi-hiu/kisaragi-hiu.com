#!/bin/bash

bash build.sh publish

cd ~/public || exit 1
git init

git config user.name "Built on Travis CI"
git config user.email "flyingfeather1501@gmail.com"

git add .
git commit -m "Deploy to Github Pages"

git push -u https://${GH_TOKEN}@github.com/flyungfeather1501/flyingfeather1501.github.io.git master --force --quiet >/dev/null 2>/dev/null
