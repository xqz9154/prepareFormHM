# download and crop DEM using the boundary of your study area.
# This can be done either by drawing the boundary on the map or by uploading a shapefile.After clipping the DEM data to the polygon boundary, 
# you can then download the clipped DEM data.


#
downloadDEM_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("message")),
    selectInput(ns("source"), "Select DEM Source", choices = c("aws", "gl3", "gl1", "alos", "srtm15plus")),
    fileInput(
      ns("shapefile"),
      "Upload Shapefile",
      accept = c('.shp', '.dbf', '.sbn', '.sbx', '.shx', '.prj'),
      multiple = TRUE
    ),
    leafletOutput(ns("mymap")),
    textInput(ns("directory"), "Enter directory for saving shapefile:", value = getwd()),
    downloadButton(ns("saveShapefile"), "Save Shapefile"),
    textInput(ns("filename"), "Enter filename for DEM:", value = "cropped_dem"),
    downloadButton(ns("downloadData"), "Download DEM")
    
  )
}


#
downloadDEM_server <- function(id, uploadedShapefile,drawnPolygon) {
  moduleServer(id, function(input, output, session) {
    output$message <- renderText({
      if (is.null(uploadedShapefile$value) && is.null(drawnPolygon$drawnPolygon)) {
        "Please draw a polygon or upload a shapefile before downloading the DEM."
      } else {
        ""
      }
    })
    
    # Add server logic for drawing a polygon
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addDrawToolbar()
    })
    
   
    ## Add server logic for drawing a polygon on map
    observeEvent(input$mymap_draw_new_feature, {
      coords <- input$mymap_draw_new_feature$geometry$coordinates
      points <- unlist(coords, recursive = TRUE)
      mtx <- matrix(points, ncol = 2, byrow = TRUE)
      polygon <- st_polygon(list(mtx)) %>% st_sfc(crs = 4326)
      drawnPolygon$drawnPolygon <- polygon
     
    })
    # save the drawn polygon to input directory
    output$saveShapefile <- downloadHandler(
      filename = function() {
        paste(input$filename, ".shp", sep = "")
      },
      content = function(file) {
        # Save the drawn polygon to a shapefile
        if (!is.null(drawnPolygon$drawnPolygon) && !is.null(input$directory)) {
          st_write(
            drawnPolygon$drawnPolygon,
            dsn = input$directory,
            layer = 'drawn_polygon',
            driver = "ESRI Shapefile",
            append = FALSE
          )
          
        }
      }
    )
    
    ### Add server logic for uploading a shapefile
    observeEvent(input$shapefile,{
      shpdf <- input$shapefile
      if (is.null(shpdf)) {
        return(NULL)
      }
       
      previouswd <- getwd()
      uploaddirectory <- dirname(shpdf$datapath[1])
      setwd(uploaddirectory)
      for (i in 1:nrow(shpdf)) {
        if (file.exists(shpdf$datapath[i])) {
        file.rename(shpdf$datapath[i], shpdf$name[i])
        }
      }
     
      # read uploaded shapefile
      uploadedShapefile$value <-
        read_sf(paste(uploaddirectory, shpdf$name[grep(pattern = "*.shp$", shpdf$name)], sep =
                        "/"))
      setwd(previouswd)
      # Reproject the shapefile to a coordinate system that uses longitude values within the range of -180 to 180 degrees
      uploadedShapefile$value <- st_transform(uploadedShapefile$value, 4326)
      
      # Add polygons from the uploaded shapefile to the map
      output$mymap <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          addDrawToolbar() %>%
          addPolygons(data = uploadedShapefile$value, color = "blue", stroke = 1, opacity = 0.8)
          })   
    })
    
    
    
    # download DEM for study area
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$filename, ".tif", sep = "")
      },
      content = function(file) {
        # Check if a polygon has been drawn or a shapefile has been uploaded
        boundary <- if (!is.null(uploadedShapefile$value)) {
          uploadedShapefile$value
        } else {
          # If a polygon has been drawn, use it as the boundary
          drawnPolygon$drawnPolygon
        }
        
        dem <-
          elevatr::get_elev_raster(boundary, source = input$source, z = 10)
        cropped_dem <- raster::crop(dem, boundary)
        raster::writeRaster(cropped_dem, input$directory, format = "GTiff",overwrite =TRUE)
        print(paste0("DEM file is save to directory:",input$directory))
        print(paste0("filename",file))
        }
    )
    
  })
}
