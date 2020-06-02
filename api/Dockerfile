FROM rocker/r-ver:3.6.0

LABEL maintainer="Farkhan <farkhan.novianto@gmail.com>"


# install the linux libraries needed for plumber
RUN apt-get update -qq && apt-get install -y \
  git-core \
  libssl-dev \
  libcurl4-gnutls-dev \
  libpq-dev \
  libxml2-dev \
  libbz2-dev \
  libicu-dev \
  liblzma-dev \
  sudo \
  bash \
  git \
  wget \
  vim \
  ssh

## Install R Packages
RUN  . /etc/environment \
    && install2.r --error \
    --repos 'http://www.bioconductor.org/packages/release/bioc' \
    --repos $MRAN \ 
    #--deps TRUE \
    dplyr plumber jsonlite

## Install R Packages for model
RUN  . /etc/environment \
    && install2.r --error \
    --repos 'http://www.bioconductor.org/packages/release/bioc' \
    --repos $MRAN \ 
    #--deps TRUE \
    rpart
    
EXPOSE 8000
WORKDIR /root
ENTRYPOINT ["R", "-e", "pr <- plumber::plumb(rev(commandArgs())[1]); pr$run(host='0.0.0.0', port=8000, swagger=TRUE)"]
CMD ["/usr/local/lib/R/site-library/plumber/examples/04-mean-sum/plumber.R"]