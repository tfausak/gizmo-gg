#!/bin/sh
set -o errexit -o xtrace

WEB_HOOK_ID=$1
WEB_HOOK_TOKEN=$2

curl \
  --request POST \
  --data '{ "content": "Deployed gizmo.gg!" }' \
  "https://discordapp.com/api/webhooks/$WEB_HOOK_ID/$WEB_HOOK_TOKEN"
