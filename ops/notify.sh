#!/bin/sh
set -o errexit -o xtrace

DISCORD_ID="$1"
DISCORD_TOKEN="$2"
MESSAGE="$3"

curl \
  --request POST \
  --data '{ "content": "'"$MESSAGE"'" }' \
  "https://discordapp.com/api/webhooks/$DISCORD_ID/$DISCORD_TOKEN"
