#!/bin/sh
set -e
deploy_url="${PDF_FOOBAR_URL:?Please set PDF_FOOBAR_URL to the URL of your server}"
nix-build -A docker-image
docker load -i result
docker push cocreature/pdf-foobar
ssh "${deploy_url}" 'docker pull cocreature/pdf-foobar && systemctl restart pdf-foobar && systemctl status pdf-foobar'
