#!/bin/bash

cd public || exit 1
git init

git config user.name "Built on Travis CI"
git config user.email "flyingfeather1501@gmail.com"

git add .
git commit -m "Deploy to Github Pages"

git push --force --quiet "https://${GH_TOKEN}@${GH_REF}" master >/dev/null 2>/dev/null
