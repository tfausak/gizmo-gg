#!/bin/sh
set -o errexit -o xtrace

DISCORD_ID="$1"
DISCORD_TOKEN="$2"

curl \
  --request POST \
  --data '{ "content": "Deployed the site!" }' \
  "https://discordapp.com/api/webhooks/$DISCORD_ID/$DISCORD_TOKEN"
