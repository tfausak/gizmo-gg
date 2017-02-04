# Ops

gizmo.gg is deployed to a DigitalOcean droplet. Its IP address is
`138.197.83.137`. To deploy changes, get the password from Taylor and log in as
`root`. Then run `./deploy.sh`. Then you're done.

This directory contains script for managing DigitalOcean droplets.

- [`provision.sh`](./provision.sh): Takes a new droplet and installs everything
  we need to run gizmo.gg on it. Should only be run once, but also should be
  harmless to run multiple times.

- [`deploy.sh`](./deploy.sh): Takes a provisioned droplet and deploys the
  gizmo.gg repo on it. This script requires two arguments: a GitHub user name
  and a [personal access token][]. For example:

  ``` sh
  ./deploy.sh 'tfausak' 'd89...'
  ```

- [`notify.sh`](./notify.sh): Notifies our development Discord channel that the
  app has been deployed. This script requires two arguments: a Discord web hook
  ID and a Discord web hook token. For example:

  ``` sh
  ./notify.sh '277...' 'wme...'
  ```

[personal access token]: https://help.github.com/articles/creating-an-access-token-for-command-line-use/
