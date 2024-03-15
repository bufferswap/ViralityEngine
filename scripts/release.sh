#!/bin/sh
# A simple script to mark a new release and push it remotely.

if [ -z "$1" ]; then
  echo "No tag argument supplied."
  echo "Usage: ./release.sh <tag>"
  exit 1
fi

TAG="$1"

git tag "$TAG"
git push origin "$TAG"
