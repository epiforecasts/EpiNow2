FROM  rocker/geospatial:latest


RUN apt-get update -y && \
    apt-get install -y \
    libudunits2-dev \
    libgdal-dev \
    libqpdf-dev \
    libmagick++-dev \
    && apt-get clean

## Copy files to working directory of server
ADD . /home/rstudio/EpiNow


## Set working directory to be this folder
WORKDIR /home/rstudio/EpiNow

## Install missing packages
RUN Rscript -e "devtools::install_dev_deps()"
