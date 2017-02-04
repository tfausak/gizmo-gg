# Ops

gizmo.gg is deployed to a DigitalOcean droplet. Its IP address is
`138.197.83.137`. To deploy changes, get the password from Taylor and log in as
`root`. Then run `./deploy.sh`. Then you're done.

This directory contains script for managing DigitalOcean droplets.

- [`provision.sh`](./provision.sh): Takes a new droplet and installs everything
  we need to run gizmo.gg on it. Should only be run once, but also should be
  harmless to run multiple times.

- [`clone.sh`](./clone.sh): Clones the repo onto disk. Takes three arguments:
  the path to clone to, a GitHub user name, and a [personal access token][].
  For example:

  ``` sh
  sh clone.sh /root/gizmo-gg 'tfausak' 'd89...'
  ```

- [`deploy.sh`](./deploy.sh): Takes a provisioned droplet and deploys the
  gizmo.gg repo on it. This script requires one argument: the path to the repo
  on disk.

  ``` sh
  sh deploy.sh /root/gizmo-gg
  ```

- [`notify.sh`](./notify.sh): Notifies our development Discord channel that the
  site has been deployed. This script requires two arguments: a Discord web
  hook ID and a Discord web hook token. For example:

  ``` sh
  sh notify.sh '277...' 'wme...'
  ```

- [`cron.sh`](./cron.sh): A script to be run *by* `cron`. This will check for
  changes and deploy if there are any. This script takes five arguments: the
  repo directory, the GitHub user name, the personal access token, the Discord
  web hook ID, and the Discord web hook token. For example:

  ``` sh
  sh cron.sh /root/gizmo-gg 'tfausak' 'd89...' '277...' 'wme...'
  ```

  This script is **not enabled by default**. You must manually edit the crontab
  on the server with `crontab -e`. You should probably run it every minute, so
  it will look like this:

  ```
  */5 * * * * env PATH="/usr/local/bin:$PATH" /root/gizmo-gg/ops/cron.sh /root/gizmo-gg 'tfausak' 'd89...' '277...' 'wme...'
  ```

[personal access token]: https://help.github.com/articles/creating-an-access-token-for-command-line-use/
