# r code to draw a polygon on leaflet map and save to local folder in a shiny app exclude rgdal package

# install.packages(c("shiny","geojsonio", "leaflet", "leaflet.extras"))
library(tidyverse)
library(shiny)
library(sf)
library(leaflet)
library(leaflet.extras)
 

ui <- fluidPage(
  leafletOutput("mymap"),
  downloadButton('downloadData', 'Download Shp')
)


   
 
server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addDrawToolbar(polygonOptions = drawPolygonOptions())
  })
  
  polygons <- reactiveValues()
  
  observeEvent(input$mymap_draw_new_feature, {
    coords <- input$mymap_draw_new_feature$geometry$coordinates
    coords
    # Flatten the coords list
    points <- unlist(coords, recursive = TRUE)
    print(points)
    # convert to matrix
    mtx <- matrix(points, ncol = 2, byrow = TRUE)
    print(mtx)
    # convert to polygon
    polygon <- st_polygon(list(mtx)) %>% st_sfc(crs = 4326)
    print(class(polygon))
    
    polygons$polygon <- polygon
    print(polygons$polygon)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("polygon", Sys.Date(), ".geojson", sep="")
    },
    content = function(file) {
      st_write(polygons$polygon, file, driver = "GeoJSON")
    }
  )
  
}

shinyApp(ui=ui,server=server)

