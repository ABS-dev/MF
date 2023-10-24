FROM rocker/tidyverse:3.4.4
MAINTAINER Thomas Kent <thomas.kent@usda.gov>
RUN R -e "devtools::install_github('ABS-dev/MF')"
