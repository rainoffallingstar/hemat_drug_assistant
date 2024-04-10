#dockerfile
# build image 
FROM rocker/shiny

# create location for app
RUN mkdir /home/shiny-app && \
    R -e 'install.packages("pak")' && \
    R -e 'pak::pkg_install(c("shiny","shinyWidgets","readxl","fs","dplyr","tidyr","stringr","bslib","thematic"),
    upgrade = TRUE,ask= FALSE, dependencies = NA)'
RUN mkdir /deploy
ADD . /deploy
WORKDIR /deploy
RUN R -e 'pak::pak("rainoffallingstar/mellon")'
RUN rm -rf /deploy
EXPOSE 8180
CMD R -e "options('shiny.port'=8180,shiny.host='0.0.0.0');library(mellon);mellon::hemat_assistant()"



