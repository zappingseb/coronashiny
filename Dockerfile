FROM rocker/tidyverse
MAINTAINER Sebastian Engel-Wolf (sebastian@mail-wolf.de)

# install R package dependencies
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libxml2-dev \
    ## clean up
    && apt-get clean \ 
    && rm -rf /var/lib/apt/lists/ \ 
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
    
## Install packages from CRAN
RUN install2.r --error \ 
    -r 'http://cran.rstudio.com' \
    ggplot2 \
    rlang \
    stringr \
    xml2 \
    plotly \
    RColorBrewer \
    ## clean up
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN git clone https://github.com/CSSEGISandData/COVID-19.git

ADD ./* $HOME/src/

WORKDIR $HOME/src/

RUN R start_script.R
