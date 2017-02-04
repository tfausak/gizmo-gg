#!/bin/sh
set -o errexit -o xtrace

# Install dependencies.
apt update
apt install --assume-yes curl nginx ufw

# Set up firewall.
yes | ufw enable
ufw allow OpenSSH
ufw allow 'Nginx HTTP'

# Configure Nginx.
rm --force /etc/nginx/sites-enabled/default
cat <<EOF | tee /etc/nginx/sites-available/gizmo-gg
server {
  listen 80 default_server;
  server_name _;
  client_max_body_size 10M;
  location / {
    proxy_pass http://127.0.0.1:8080;
  }
}
EOF
ln --force --symbolic /etc/nginx/sites-available/gizmo-gg /etc/nginx/sites-enabled/gizmo-gg
systemctl restart nginx

# Install Docker.
curl 'https://yum.dockerproject.org/gpg' | apt-key add -
echo 'deb https://apt.dockerproject.org/repo/ debian-jessie main' | tee /etc/apt/sources.list.d/docker.list
apt update
apt install --assume-yes docker-engine

# Install Docker Compose.
curl --location 'https://github.com/docker/compose/releases/download/1.10.1/docker-compose-Linux-x86_64' --output /usr/local/bin/docker-compose
chmod +x /usr/local/bin/docker-compose
