#!/bin/bash
echo "Running docker elfeck/elfeckcom with specified version" $1
docker run -it -p 3000:3000 -v ~/projects/web/elfeckcom:/opt/elfeckcom \
       elfeck/elfeckcom:$1 bash
