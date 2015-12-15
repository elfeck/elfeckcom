#!/bin/bash
version=$(cat "dockerversion.txt")
echo "starting up docker with current version: $version"
docker run -d 	-v /home/seb/elfeckcom/db:/var/elfeckcom/private/db \
		-v /home/seb/elfeckcom/config.txt:/var/elfeckcom/private/config.txt \
		-v /home/seb/whyiliketrees/js:/var/elfeckcom/public/static/games/whyiliketrees \
		-v /home/seb/LD29/build/web:/var/elfeckcom/public/static/games/LD29 \
		-p 80:3000 elfeck/elfeckcom:$version
