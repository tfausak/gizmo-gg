#!/bin/sh
set -o errexit -o xtrace

# Where to clone the repo on disk.
REPO_DIR="$1"

# How to authenticate with GitHub.
GITHUB_USER="$2"
GITHUB_TOKEN="$3"

# Which repo to clone.
GITHUB_REPO=tfausak/gizmo-gg

# Make sure the repo exists where we expect it to.
if ! test -d "$REPO_DIR"
then
  git clone "https://$GITHUB_USER:$GITHUB_TOKEN@github.com/$GITHUB_REPO.git" "$REPO_DIR"
fi
