# revise the code to avoid Error in uploadedShapefile$value: could not find function "uploadedShapefile$value"

library(shiny)
library(pacman)
p_load(tidyverse,
       tools,
       elevatr,
       terra,
       sf,
       whitebox,
       tmap,
       stars,
       leaflet,
       leaflet.extras)


#
ui <- fluidPage(
  titlePanel("Download DEM for study area"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "shapefile",
        "Upload Shapefile",
        accept = c('.shp', '.dbf', '.sbn', '.sbx', '.shx', '.prj', ".geojson"),
        multiple = TRUE
      ),
      selectInput(
        "source",
        "Select DEM Source",
        choices = c("aws", "gl3", "gl1", "alos", "srtm15plus")
      )
      
    ),
    mainPanel(
      leafletOutput("mymap"),
      downloadButton('savePolygon', 'Save Drawn Polygon'),
      downloadButton("downloadData", "Download DEM"),
      textInput("filename", "Enter filename for DEM:", value = "cropped_dem")
      
    )
  )
)

server <- function(input, output, session) {
  
  # option to upload shapefile from local folder -------------
  # Define 'uploadedShapefile' as a reactive value outside of the 'observe' block
  uploadedShapefile <- reactiveValues(value = NULL)
  # Read the uploaded shapefile
  # adapted from https://community.rstudio.com/t/shinyfiles-and-shapefiles/89099/5
  observe({
    shpdf <- input$shapefile
    if (is.null(shpdf)) {
      return()
    }
    previouswd <- getwd()
    uploaddirectory <- dirname(shpdf$datapath[1])
    setwd(uploaddirectory)
    for (i in 1:nrow(shpdf)) {
      file.rename(shpdf$datapath[i], shpdf$name[i])
    }
    setwd(previouswd)
    # print(uploaddirectory)
    # print(shpdf$name)
    # read-in shapefile
    uploadedShapefile$value <-
      read_sf(paste(uploaddirectory, shpdf$name[grep(pattern = "*.shp$", shpdf$name)], sep =
                      "/"))
  })
  
  # show basemap ---------
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addDrawToolbar(polygonOptions = drawPolygonOptions())
  })
  # upload polygon file and update the map 
  observeEvent(uploadedShapefile$value, {
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addDrawToolbar(polygonOptions = drawPolygonOptions()) %>%
        addPolygons(data = uploadedShapefile$value)
    })
  })
  
  # option to draw polygon on map
  # drawing polygon and save to local folder -------
  drawnPolygon <- reactiveValues(drawnPolygon = NULL)
  observeEvent(input$mymap_draw_new_feature, {
    coords <- input$mymap_draw_new_feature$geometry$coordinates
    points <- unlist(coords, recursive = TRUE)
    mtx <- matrix(points, ncol = 2, byrow = TRUE)
    polygon <- st_polygon(list(mtx)) %>% st_sfc(crs = 4326)
    drawnPolygon$drawnPolygon <- polygon
    # print(class(drawnPolygon$drawnPolygon))
  })
  # observeEvent(input$savePolygon, {
  #   # Write the drawn polygon to a shapefile
  #   sf::st_write(drawnPolygon$drawnPolygon, "drawn_polygon.shp")
  # })
  # # save the drawn polygon to local folder
  output$savePolygon <- downloadHandler(
    filename = function() {
      # paste("polygon", Sys.Date(), ".geojson", sep="")
      paste("polygon", Sys.Date(), ".shp", sep = "")
    },
    content = function(file) {
      #     st_write(drawnPolygon$drawnPolygon, file, driver = "GeoJSON")
      sf::st_write(drawnPolygon$drawnPolygon, file)
    }
  )
  
  
  # download DEM data for specific region ---------
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$filename,Sys.Date(),".tif", sep = "")
    },
    content = function(file) {
      # Get the bounding box from the uploaded shapefile or the drawn polygon
      bbox <- if (any(!is.null(uploadedShapefile$value))) {
        uploadedShapefile$value
      } else {
        drawnPolygon$drawnPolygon
      }
      
      
      # Download DEM data from OpenTopography
      dem <-
        elevatr::get_elev_raster(bbox, source = input$source, z = 10)
      
      # Crop the DEM data to the bounding box of the upload shapefile or drawn polygon
      cropped_dem <- raster::crop(dem, bbox)
      
      # Write the cropped DEM data to a GeoTIFF file
      raster::writeRaster(cropped_dem, file, format = "GTiff")
    }
  )
}

shinyApp(ui = ui, server = server)
