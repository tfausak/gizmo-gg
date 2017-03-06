#!/bin/sh
set -o errexit -o xtrace

# Where the repo has been cloned.
REPO_DIR="$1"

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
env PALADIN_CONNECT="$API_URL" docker-compose up -d --force-recreate --remove-orphans
