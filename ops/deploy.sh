#!/bin/sh
set -o errexit -o xtrace

# How to authenticate with GitHub.
GITHUB_USER=$1
GITHUB_TOKEN=$2

# Which repo to clone.
GITHUB_REPO=tfausak/gizmo-gg

# Where to clone the repo on disk.
REPO_DIR=/root/gizmo-gg

# Make sure the repo exists where we expect it to.
if ! test -d "$REPO_DIR"
then
  git clone "https://$GITHUB_USER:$GITHUB_TOKEN@github.com/$GITHUB_REPO.git" "$REPO_DIR"
fi

# Bring the repo up to date.
cd "$REPO_DIR"
git reset --hard origin/master
git pull

# Build all the Docker containers.
API_URL=http://gizmo.gg/api
env MERC_API_URL="'$API_URL/'" docker-compose build

# Start the new Docker containers.
docker-compose up -d postgres
sleep 5
env PALADIN_CONNECT="$API_URL" docker-compose up -d --remove-orphans
