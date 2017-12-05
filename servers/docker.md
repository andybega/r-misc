Docker cheatsheet
================

Pull the Rstudio image from Docker hub.

``` bash
docker pull rocker/rstudio
docker images
docker images -a

# to clean
docker images rmi <imageID>
```

Start/stop container

``` bash
# run with root to be able to install dependencies; safe on local only probably
docker run -dp 8787:8787 -e ROOT=TRUE rocker/rstudio

# on a remote server, don't use default logins
USER=[user]
PW=[pw]
docker run -dp 8787:8787 -e ROOT=TRUE -e USER=$USER -e PASSWORD=$PW rocker/rstudio

docker ps
docker stop <imageID>
docker ps -a
docker rm <imageID>
docker start <imageID>
```

Access Rstudio at "localhost:8787", default credentials are "rstudio".

Installing system dependencies with `apt-get` requires ssh-ing into the container, cannot be done from Rstudio terminal.

``` bash
docker exec -it <imageID> bin/bash
apt-get update
apt-get install python3-pip
pip3 install docopt
```
