FROM rocker/shiny
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
    dplyr \
    ggplot2 \
    rlang \
    xml2 \
    ## clean up
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

## Install packages from CRAN
RUN install2.r --error \ 
    -r 'http://cran.rstudio.com' \
    rvest \
    tidyverse \
    ## clean up
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
## assume shiny app is in build folder /shiny
COPY ./shiny/ /srv/shinyapps/corona/