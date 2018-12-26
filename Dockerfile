# não finalizado
FROM rocker/rstudio-stable:3.4.3

RUN apt-get update
RUN apt-get install -y libssl-dev cmake libxml2-dev
