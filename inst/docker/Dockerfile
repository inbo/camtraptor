# reference: https://github.com/gbif/camtrap-dp-pipeline/blob/master/docker/Dockerfile

FROM r-base:4.2.2

# Install required system packages
RUN apt-get update \
    && apt-get -y install build-essential libxml2-dev libssl-dev libcurl4-openssl-dev libssl-dev \
                          libsodium-dev software-properties-common wget libpq-dev libssl-dev libjq-dev libv8-dev

# Install Apache Arrow libraries
RUN wget https://apache.jfrog.io/artifactory/arrow/$(lsb_release --id --short | tr 'A-Z' 'a-z')/apache-arrow-apt-source-latest-$(lsb_release --codename --short).deb -O /tmp/apache-arrow-apt-source-latest-$(lsb_release --codename --short).deb \
    && apt-get install -y -V /tmp/apache-arrow-apt-source-latest-$(lsb_release --codename --short).deb \
    && apt-get update -y \
    && apt-get install -y libarrow-dev

# Install required R packages
RUN apt-get -y install r-cran-plumber r-cran-assertthat r-cran-dbi r-cran-dplyr r-cran-glue r-cran-htmltools r-cran-lifecycle \
                       r-cran-lubridate r-cran-purrr r-cran-rcolorbrewer r-cran-readr r-cran-rlang r-cran-rsqlite r-cran-stringr \
                       r-cran-tidyr r-cran-uuid r-cran-covr r-cran-knitr r-cran-rmarkdown r-cran-testthat r-cran-knitr r-cran-pkgdown \
                       r-cran-roxygen2 r-cran-rversions r-cran-urlchecker r-cran-here r-cran-remotes r-cran-lifecycle r-cran-colorspace \
                       r-cran-labeling r-cran-farver r-cran-isoband r-cran-gtable r-cran-viridis r-cran-terra r-cran-sp r-cran-curl \
                       r-cran-evaluate r-cran-purrr r-cran-png r-cran-markdown r-cran-crosstalk r-cran-htmlwidgets r-cran-tidyselect \
                       r-cran-munsell r-cran-gridextra r-cran-raster r-cran-scales r-cran-vroom r-cran-v8 r-cran-jsonld r-cran-ggplot2 \
                       r-cran-proxy r-cran-rematch r-cran-jpeg r-cran-matrix r-cran-e1071 r-cran-wk r-cran-dbplyr r-cran-listenv r-cran-parallelly \
                       r-cran-classint r-cran-hunspell r-cran-redland r-cran-globals r-cran-palmerpenguins r-cran-maps r-cran-rgdal r-cran-s2 \
                       r-cran-future r-cran-units r-cran-shinyjs r-cran-spelling r-cran-rdflib r-cran-dbitest r-cran-maptools r-cran-xml \
                       r-cran-mockr r-cran-sf r-cran-devtools

# Installing source packages
RUN mkdir -p /R/dwca /R/dp
WORKDIR "/R/"
COPY ["R/install_packages.R", "/R/"]
RUN ["Rscript", "install_packages.R"]

COPY ["R/convert-dp-dwca.R", "R/resources/meta.xml", "/R/"]

# General configs
VOLUME ["/R/dwca/", "/R/dp/", "/R/logs/"]
ENTRYPOINT ["Rscript", "convert-dp-dwca.R"]