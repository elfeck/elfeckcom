#!/bin/bash

version=$(cat "/home/seb/elfeckcom/docker/dockerversion.txt")

echo "Starting up elfeckcom with current version: $version ..."
docker run -d 	-v /home/seb/elfeckcom/db:/var/elfeckcom/private/db \
		-v /home/seb/elfeckcom/config.txt:/var/elfeckcom/private/config.txt \
		-v /home/seb/elfeckcom/log:/var/elfeckcom/private/log \
		-v /home/seb/whyiliketrees/js:/var/elfeckcom/public/static/games/whyiliketrees \
		-v /home/seb/LD29/build/web:/var/elfeckcom/public/static/games/LD29 \
		--expose=3000 \
		-e VIRTUAL_HOST=www.elfeck.com,elfeck.com \
		-e LETSENCRYPT_HOST=elfeck.com,www.elfeck.com \
                -e LETSENCRYPT_EMAIL=kreisel.sebastian@gmail.com \
		elfeck/elfeckcom:$version
