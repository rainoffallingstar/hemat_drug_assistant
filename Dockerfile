# Multi-stage build: Go engine + Shiny app
# Build Go engine
FROM golang:1.22 AS go-builder
WORKDIR /src
COPY go.mod go.sum ./
RUN go mod download
COPY cmd/ ./cmd/
COPY internal/ ./internal/
RUN CGO_ENABLED=0 GOOS=linux GOARCH=amd64 go build -o /mellon-engine ./cmd/mellon-engine/

# Build Shiny app
FROM rocker/shiny:latest
RUN R -e 'install.packages("pak")' && \
    R -e 'pak::pkg_install(c("shiny","shinyWidgets","dplyr","tidyr","stringr","bslib","thematic","jsonlite","processx"),upgrade = TRUE, ask = FALSE, dependencies = NA)'

RUN mkdir -p /srv/shiny-server/mellon
COPY --from=go-builder /mellon-engine /srv/shiny-server/mellon/inst/bin/mellon-engine
COPY R/ /srv/shiny-server/mellon/R/
COPY inst/ /srv/shiny-server/mellon/inst/
COPY data/ /srv/shiny-server/mellon/data/
COPY DESCRIPTION NAMESPACE /srv/shiny-server/mellon/

WORKDIR /srv/shiny-server/mellon
RUN R -e 'remotes::install_local(force = TRUE, dependencies = FALSE)'

EXPOSE 8180
CMD R -e "options('shiny.port'=8180, shiny.host='0.0.0.0'); library(mellon); mellon::hemat_assistant()"
