# load_library.R
# install.packages("pacman")
library(pacman)
p_load(shiny,
       shinyFiles,
       tidyverse,
       tools,
       elevatr,
       terra,
       raster,
       sf,
       whitebox,
       tmap,
       stars,
       leaflet,
       leafem,
       leaflet.extras,
       geodrawr)
whitebox::wbt_init()