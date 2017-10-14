#!/bin/bash

bash build.sh publish

cd ~/public || exit 1
git init

git config user.name "Built on Travis CI"
git config user.email "flyingfeather1501@gmail.com"

git add .
git commit -m "Deploy to Github Pages"

git remote add deploy "https://${GH_TOKEN}@${GH_REF}"
git push -u deploy master --force --quiet >/dev/null 2>/dev/null
