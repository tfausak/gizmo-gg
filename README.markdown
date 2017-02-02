# [gizmo.gg][]

gizmo.gg is split into a few services:

- [Venom](./venom): The top-level HTTP server.
- [Merc](./merc): The front end.
- [Paladin](./paladin): The back end.

## Deploy

gizmo.gg is deployed to a DigitalOcean droplet. Its IP address is
`138.197.83.137`. To deploy changes, get the password from Taylor and log in as
`root`. The run `./deploy.sh`. Then you're done.

Although it's already done, here's how to set up the machine in the first
place:

``` sh
# Install Docker.
sudo apt update
sudo apt install apt-transport-https ca-certificates curl "linux-image-extra-$(uname -r)" linux-image-extra-virtual
curl --fail --location --show-error --silent https://yum.dockerproject.org/gpg | sudo apt-key add -
sudo add-apt-repository "deb https://apt.dockerproject.org/repo/ ubuntu-$(lsb_release -cs) main"
sudo apt-get update
apt install docker-engine

# Install Docker Compose.
sudo curl -o /usr/local/bin/docker-compose -L "https://github.com/docker/compose/releases/download/1.10.1/docker-compose-$(uname -s)-$(uname -m)"
sudo chmod +x /usr/local/bin/docker-compose

# Enable the firewall.
ufw enable
ufw allow OpenSSH
ufw allow 'Nginx HTTP'

# Install nginx.
sudo apt update
sudo apt install nginx

# Configure nginx.
sudo rm -f /etc/nginx/sites-enabled/default
cat <<EOF | sudo tee /etc/nginx/sites-available/gizmo-gg
server {
  listen 80 default_server;
  server_name _;
  client_max_body_size 10M;
  location / {
    proxy_pass http://127.0.0.1:8080;
  }
}
EOF
sudo rm -f /etc/nginx/sites-enabled/gizmo-gg
sudo ln -s /etc/nginx/sites-available/gizmo-gg /etc/nginx/sites-enabled/gizmo-gg
sudo systemctl restart nginx

# Clone the project.
GITHUB_TOKEN='...' # Make a personal access token.
REPO_DIR='/root/gizmo-gg'
git clone "https://tfausak:$GITHUB_TOKEN@github.com/tfausak/gizmo-gg.git" "$REPO_DIR"
cd "$REPO_DIR"

# Build the project.
API_URL='http://gizmo.gg/api'
env MERC_API_URL="'$API_URL/'" docker-compose build

# Run the project.
docker-compose up -d postgres
sleep 1
env PALADIN_CONNECT="$API_URL" docker-compose up -d --remove-orphans
```

[gizmo.gg]: http://gizmo.gg
