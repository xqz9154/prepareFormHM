# First tabPanel to choose the DEM file from local folder or 
# download using 00-download.R file to download DEM
# then show the DEM map on main mainPanel
# Second to fill the DEM depends on the fill sinks method ("Wang and Liu" or "Least cost")
library(shiny)
library(pacman)
p_load(tidyverse,
       tools,
       terra,
       raster,
       sf,
       profvis,
       whitebox,
       tmap,
       stars,
       leaflet,
       leafem)


# ui ----------
ui <- fillPage(
  tags$style(type = "text/css", "html, body {width:100%; height:100%}"),
  navbarPage(
    "Hydrological Model Catchment Delineation",
    tabPanel("DEM preprocess", #download from SRTM or upload from local folder
             sidebarLayout(
               sidebarPanel(
                 # Add a file input for the user to select the DEM dataset
                 fileInput(
                   "dem",
                   "Select DEM Dataset",
                   accept = c(".tif",
                              ".geotiff",
                              '.hgt'),
                   multiple = TRUE
                 ) 
                   
               ), 
               mainPanel(leafletOutput(
                 "map", width = "100vw", height = "100vh"
               ))
             )),
    # tabPanel
    tabPanel("Fill sinks",
             sidebarLayout(sidebarPanel(
               # Add a button to open the dialog box to select folder where save dem file
               actionButton("selectFolder", "Select Folder"),
               
               # Add a radio button for the depression method
               radioButtons("depressionMethod", "Select Depression Method",
                            choices = list("Wang and Liu" = "wang_liu", "Least Cost" = "least_cost"),
                            selected = "wang_liu")
               
               # downloadButton("save_output", "Save Output")
             ),
             mainPanel(
               leafletOutput("map", width = "100vw", height = "100vh")
             ))), 
    # 
    tabPanel("Flow accumulation", "This panel is missing"),
    tabPanel("Flow direction", "This panel is missing"),
    tabPanel("Extract streams", "This panel is missing"),
    tabPanel("Watershed", "This panel is missing")
    
  )
)


#
server <- function(input, output, session) {
  # profvis({
  observeEvent(input$dem, {
    req(input$dem)
    # Initialize an empty list to store the raster objects
    rasters <- list()
    
    # Loop over each input file
    if (length(input$dem) > 1) {
      # 
      rasters <- lapply(input$dem$datapath, function(path) {
        file_ext <- tools::file_ext(path)
        if (file_ext %in% c("tif", "asi", "geotiff")) {
          rasters <- terra::rast(path)
        } else if(file_ext %in% c("hgt")) {
          # Create a RasterLayer object
          raster_layer <- raster::raster(path)
          # Convert the RasterLayer object to a SpatRaster object
          rasters <- terra::rast(raster_layer)
        }
      })
    }
    else {
      # If input file contains only one file, read the file and add it to the rasters list
      file_ext <- tools::file_ext(input$dem$datapath)
      if (file_ext == "tif") {
        rasters[[1]] <- terra::rast(input$dem$datapath)
      } else if (file_ext == "hgt") {
        raster_layer <- raster::raster(input$dem$datapath)
        rasters[[1]] <- terra::rast(raster_layer)
      } else if (file_ext == "geotiff") {
        rasters[[1]] <- terra::rast(input$dem$datapath)
      }
    }
    
    
    # Print rasters
    print(rasters)
    # Open the dialog box when the button is clicked
    observeEvent(input$selectFolder, {
      # Open the dialog box
      output$folder <- renderText({
        folder <- fileDialog(
          title = "Select Folder",
          multiple = FALSE,
          chooseDir = TRUE
        )
        # Return the selected folder
        folder
      })
    })
    
    # If there are multiple raster files selected, merge them together
    if (length(rasters) > 1) {
      merge_rasters <- do.call(terra::merge, rasters)
      
      # resample to lower resolution
      fact <- round(dim(merge_rasters)[1:2] / dim(rasters[[1]])[1:2])
      merge_rasters <- terra::aggregate(merge_rasters, fact, mean)
      
      # Print rasters
      print(merge_rasters)
      # Check if the raster data is correctly loaded
      if (is.null(rasters[[1]])) {
        return(NULL)
      }
      
      # Display a loading message
      withProgress(message = "Loading map...", {
        output$map <- renderLeaflet({
          leaflet() %>%
            addTiles(group = "Map") %>%
            addLayersControl(
              baseGroups = c("Map"),
              overlayGroups = c("Raster"),
              options = layersControlOptions(collapsed = FALSE)
            ) %>%
            addRasterImage(merge_rasters,
                           group = "Raster",
                           maxBytes = 100 * 1024 ^ 2)
        })
      })
      
      # Save the rasters to a file
      terra::writeRaster(merge_rasters, paste0(output$folder, "/rasters.tif"), overwrite = TRUE)
    }
    else #there is only one raster file selected
    {
      # Check if the raster data is correctly loaded
      if (is.null(rasters[[1]])) {
        return(NULL)
      }
      # Print rasters
      print(rasters)
      # Display a loading message
      withProgress(message = "Loading map...", {
        output$map <- renderLeaflet({
          leaflet() %>%
            addTiles(group = "Map") %>%
            addLayersControl(
              baseGroups = c("Map"),
              overlayGroups = c("Raster"),
              options = layersControlOptions(collapsed = FALSE)
            ) %>%
            addRasterImage(rasters[[1]],
                           group = "Raster",
                           maxBytes = 100 * 1024 ^ 2)
        })
      })
      
      # Save the raster to a file
      terra::writeRaster(rasters[[1]], paste0(output$folder, "/rasters.tif"), overwrite = TRUE)
    }
  })
 
  # fill sinks -----------
  observeEvent(input$dem, {
    req(input$dem)
    # Read the saved raster.tif file
    raster <- terra::rast(paste0(output$folder, "/rasters.tif"))
    # Use the selected depression method
    observeEvent(input$depressionMethod, {
      req(input$depressionMethod)
      if (input$depressionMethod == "wang_liu") {
        # Use the Wang and Liu method
        filled_raster <- whitebox::wbt_fill_depressions_wang_and_liu(
          dem = raster,
          output = "filled_raster.tif"
        )
      } else if (input$depressionMethod == "least_cost") {
        # using  least_cost method
        filled_raster <- whitebox::wbt_breach_depressions_least_cost(
          dem = raster,
          output = "filled_raster.tif",
          dist = 5,
          fill = TRUE
        ) 
      }
    })
    
    # Add the output raster to the first map
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles(group = "Map") %>%
        addLayersControl(
          baseGroups = c("Map"),
          overlayGroups = c("Raster"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        addRasterImage(filled_raster,
                       group = "Raster",
                       maxBytes = 100 * 1024 ^ 2)
    })
    output$save_output <- downloadHandler(
      filename = function() {
        paste("filled_raster", ".tif", sep="")
      },
      content = function(file) {
        file.copy("filled_raster.tif", file)
      }
    )
    
  })
}


shinyApp(ui = ui, server = server)
