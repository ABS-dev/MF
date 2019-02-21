FROM rocker/tidyverse:3.4.4
MAINTAINER Marie Vendettuoli <marie.c.vendettuoli@aphis.usda.gov>

RUN R -e "devtools::install_github('ABS-dev/MF')"