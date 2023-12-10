# shiny app to download specific region SRTM DEM dataset

library(shiny)
library(pacman)
p_load(tidyverse,
       tools,
       terra, # Replaced raster with terra
       sf,
       whitebox,
       tmap,
       stars,
       leaflet,
       leaflet.extras)

 

# 
 

ui <- fluidPage(
  titlePanel("SRTM DEM Download and Crop"),
  sidebarLayout(
    sidebarPanel(
      downloadButton("downloadData", "Download Cropped SRTM DEM"),
      downloadButton("savePolygon", "Save Drawn Polygon")
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# 
server <- function(input, output, session) {
  drawnItems <- reactiveValues(drawnItems = NULL)
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      setView(lng = 0, lat = 0, zoom = 2) %>%
      addDrawToolbar(
        editOptions = editToolbarOptions(selected = TRUE),
        polylineOptions = drawPolylineOptions(),
        polygonOptions = drawPolygonOptions(),
        circleOptions = drawCircleOptions(),
        rectangleOptions = drawRectangleOptions(),
        markerOptions = drawMarkerOptions()
      )
  })
  
  observe({
    if (!is.null(input$map_draw_new_feature)) {
      drawnItems$drawnItems <- input$map_draw_new_feature
    }
  })
  
  observeEvent(input$savePolygon, {
    if (!is.null(drawnItems$drawnItems)) {
      # Convert drawn polygon to sf object
      polygon_sf <- st_sfc(drawnItems$drawnItems)
      # Save sf object as shapefile
      st_write(polygon_sf, "./output/drawn_polygon.shp")
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("cropped_srtm_dem.tif", sep = "")
    },
    content = function(file) {
      # Download and crop SRTM DEM
      # This is a placeholder. Replace with actual code to download and crop SRTM DEM
      SRTM_dem <- raster("./data/Elbe_200m_ETRS89.tif") # Replace with actual SRTM DEM file
      cropped_dem <- crop(SRTM_dem, drawnItems$drawnItems)
      writeRaster(cropped_dem, file, format = "GTiff")
    }
  )
}





shinyApp(ui = ui, server = server)
