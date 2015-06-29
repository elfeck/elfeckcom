#!/bin/bash
version=$(cat dockerversion.txt)
version_inc=`echo $version | ( IFS=".$IFS" ; read a b && echo $a.$((b + 1)) )`
echo "previous elfeckcom version: $version"
echo "git pull and docker build ..."

cd ..
git pull origin master
docker build -t elfeck/elfeckcom:$version_inc .

if [ $? -eq 0 ]; then
	echo "incrementing dockerversion to: $version_inc"
	echo "$version_inc" > docker/dockerversion.txt
else
	echo "build error. exiting"
fi
