# draw_polygon.R
#
draw_polygon_ui <- function(id) {
  ns <- NS(id)
  tagList(leafletOutput(ns("mymap")),
          # downloadButton(ns('savePolygon'), 'Save the Drawn Polygon'),
          downloadButton(ns('download_shp'), 'Select the location to save the shapefile'))
}

#
draw_polygon_server <- function(id, drawnPolygon) {
  moduleServer(id, function(input, output, session) {
    # display the map with draw tool
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addDrawToolbar()
    })
    # draw a polygon
    observeEvent(input$mymap_draw_new_feature, {
      coords <- input$mymap_draw_new_feature$geometry$coordinates
      points <- unlist(coords, recursive = TRUE)
      mtx <- matrix(points, ncol = 2, byrow = TRUE)
      polygon <- st_polygon(list(mtx)) %>% st_sfc(crs = 4326)
      drawnPolygon$drawnPolygon <- polygon
    })
    # save the drawn polygon
    output$download_shp <- downloadHandler(
      filename = function(){
        paste("polygon", Sys.Date(), ".zip", sep = "")
      },
      content = function(file) {
        data = drawnPolygon$drawnPolygon
        # create a temp folder for shp files
        temp_shp <- getwd()
        layer_options = c("SHAPE_RESTORE_SHX"="YES")
        # write shp files
        tryCatch({
          st_write(data, dsn=paste0(temp_shp, "/temp_shp.shp"), driver = "ESRI Shapefile", config_options = layer_options)
          
           }, error = function(e) {
          print(paste0("Error writing shapefile: ", e))
          return(NULL)
        })
        # zip all the shp files
        zip_file <- file.path(temp_shp, "temp_shp.zip")
        shp_files <- list.files(temp_shp,
                                "temp_shp",
                                full.names = TRUE)
        # create the zip file
        tryCatch({
          # replace colons in the file paths with underscores
          zip_file <- gsub(":", "_", zip_file)
          shp_files <- gsub(":", "_", shp_files)
          # use absolute file paths
          zip_file <- file.path(temp_shp, "temp_shp.zip")
          shp_files <- file.path(temp_shp, "temp_shp.shp")
          zip(zip_file, shp_files)
        }, error = function(e) {
          print(paste0("Error creating zip file: ", e))
          return(NULL)
        })
        # copy the zip file to the file argument
        tryCatch({
          # print the location of the copied file
          print(paste("File copied to:", file))
          file.copy(zip_file, file)
          
        }, error = function(e) {
          print(paste0("Error copying zip file: ", e))
          return(NULL)
        })
        # remove all the files created
        tryCatch({
          # remove all the files created
          unlink(c(zip_file, shp_files), recursive = TRUE)
        }, error = function(e) {
          print(paste0("Error removing files: ", e))
          return(NULL)
        })
      }
    )
  })
}





