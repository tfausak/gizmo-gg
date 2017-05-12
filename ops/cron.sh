#!/bin/sh
set -o errexit -o xtrace

REPO_DIR="$1"
GITHUB_USER="$2"
GITHUB_TOKEN="$3"
DISCORD_ID="$4"
DISCORD_TOKEN="$5"

echo "$(date)"

sh "$REPO_DIR/ops/clone.sh" "$REPO_DIR" "$GITHUB_USER" "$GITHUB_TOKEN"

cd "$REPO_DIR"
git reset --hard origin/master
OLD="$(git rev-parse HEAD)"
git pull
NEW="$(git rev-parse HEAD)"

if test "$NEW" != "$OLD" -o "$FORCE" = 'true'
then
  sh "$REPO_DIR/ops/notify.sh" "$DISCORD_ID" "$DISCORD_TOKEN" 'Deploying `'"$NEW"'` ...'
  sh "$REPO_DIR/ops/deploy.sh" "$REPO_DIR"
  sh "$REPO_DIR/ops/notify.sh" "$DISCORD_ID" "$DISCORD_TOKEN" 'Finished deploying `'"$NEW"'`.'
else
  echo 'Not deploying. Re-run with FORCE=true to deploy.'
fi
