#!/bin/sh
set -o errexit -o xtrace

REPO_DIR="$1"
S3_ACCESS_KEY_ID="$2"
S3_SECRET_ACCESS_KEY="$3"

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
env \
  PALADIN_CONNECT="$API_URL" \
  TAKUMI_URL='http://gizmo.gg/takumi' \
  TAKUMI_ACCESS_KEY_ID="$S3_ACCESS_KEY_ID" \
  TAKUMI_SECRET_ACCESS_KEY="$S3_SECRET_ACCESS_KEY" \
  docker-compose up -d --force-recreate --remove-orphans
