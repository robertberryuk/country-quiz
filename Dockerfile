FROM rocker/r-ver:4.4.1
RUN apt-get update && apt-get install -y --no-install-recommends \
    libudunits2-dev libgdal-dev libgeos-dev libproj-dev \
    libcurl4-openssl-dev libssl-dev libxml2-dev \
    libcairo2-dev libxt-dev libpng-dev && \
    rm -rf /var/lib/apt/lists/*
RUN install2.r --error shiny leaflet sf rnaturalearth rnaturalearthdata bslib
WORKDIR /app
COPY . /app
EXPOSE 3838
CMD ["R","-e","shiny::runApp('/app', host='0.0.0.0', port=3838)"]
