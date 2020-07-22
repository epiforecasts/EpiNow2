FROM  rocker/geospatial:latest


RUN apt-get update -y && \
    apt-get install -y \
    libudunits2-dev \
    libgdal-dev \
    libqpdf-dev \
    libmagick++-dev \
    && apt-get clean

## Copy files to working directory of server
ADD . /home/rstudio/EpiNow2

## Set working directory to be this folder
WORKDIR /home/rstudio/EpiNow2

## Install missing packages
RUN Rscript -e "devtools::install_dev_deps()"
RUN Rscript -e "install.packages(c('rnaturalearthhires', 'rnaturalearthdata'), \
                                 repos = 'http://packages.ropensci.org', \
                                 type = "source")"

WORKDIR ..

## Install the local version of EpiNow2
Run R CMD INSTALL --no-multiarch --with-keep.source EpiNow2
