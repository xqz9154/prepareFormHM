# test 
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


# UI
ui <- fillPage(
  tags$style(type = "text/css", "html, body {width:100%; height:100%}"),
  navbarPage(
    "Hydrological Model Catchment Delineation",
    tabPanel("DEM preprocess", #download from source, clip using roi, reproject to roi
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
    tabPanel("Fill sinks",
             sidebarLayout(sidebarPanel(
               # Add a button to open the dialog box to select folder where save dem file
               actionButton("selectFolder", "Select Folder"),
               
               # Add a radio button for the depression method
               radioButtons("depressionMethod", "Select Depression Method",
                            choices = list("Wang and Liu" = "wang_liu", "Least Cost" = "least_cost"),
                            selected = "wang_liu")
               
             ),
             mainPanel(
               leafletOutput("map", width = "100vw", height = "100vh")
             ))), 
    tabPanel("Flow accumulation", "This panel is missing"),
    tabPanel("Flow direction", "This panel is missing"),
    tabPanel("Extract streams", "This panel is missing"),
    tabPanel("Watershed", "This panel is missing")
  )
)

# Server ----------
server <- function(input, output, session) {
  
  # Function to process uploaded DEM data
  process_dem_data <- function(dem) {
    rasters <- list()
    if (length(dem) > 1) {
      rasters <- lapply(dem$datapath, function(path) {
        file_ext <- tools::file_ext(path)
        if (file_ext %in% c("tif", "asi", "geotiff")) {
          rasters <- terra::rast(path)
        } else if(file_ext %in% c("hgt")) {
          raster_layer <- raster::raster(path)
          rasters <- terra::rast(raster_layer)
        }
      })
      print(paste("Number of rasters loaded: ", length(rasters)))
      merge_rasters <- do.call(terra::merge, rasters)
      fact <- round(dim(merge_rasters)[1:2] / dim(rasters[[1]])[1:2])
      merge_rasters <- terra::aggregate(merge_rasters, fact, mean)
      print(merge_rasters)
      if (is.null(rasters[[1]])) {
        return(NULL)
      }
    }
    else {
      file_ext <- tools::file_ext(dem$datapath)
      if (file_ext == "tif") {
        rasters[[1]] <- terra::rast(dem$datapath)
      } else if (file_ext == "hgt") {
        raster_layer <- raster::raster(dem$datapath)
        rasters[[1]] <- terra::rast(raster_layer)
      } else if (file_ext == "geotiff") {
        rasters[[1]] <- terra::rast(dem$datapath)
      }
      print(paste("Number of rasters loaded: ", length(rasters)))
      print(rasters)
      if (is.null(rasters[[1]])) {
        return(NULL)
      }
    }
    return(rasters)
  }
  
  
  # Function to display raster data on a Leaflet map
  display_raster_data <- function(rasters) {
    withProgress(message = "Loading map...", {
      output$map <- renderLeaflet({
        leaflet() %>%
          addTiles(group = "Map") %>%
          addLayersControl(
            baseGroups = c("Map"),
            overlayGroups = c("Raster"),
            options = layersControlOptions(collapsed = FALSE)
          ) %>%
          addRasterImage(rasters,
                         group = "Raster",
                         maxBytes = 100 * 1024 ^ 2)
      })
      print("Map rendered")
    })
  }
  
  
  # Function to save raster data to a file
  save_raster_data <- function(rasters, folder) {
    if (length(rasters) > 1) {
      merge_rasters <- do.call(terra::merge, rasters)
      terra::writeRaster(merge_rasters, paste0(folder, "/rasters.tif"), overwrite = TRUE)
    }
    else {
      terra::writeRaster(rasters[[1]], paste0(folder, "/rasters.tif"), overwrite = TRUE)
    }
  }
  
  # In the server function, call the above functions
  observeEvent(input$dem, {
    req(input$dem)
    rasters <- process_dem_data(input$dem)
    display_raster_data(rasters)
    save_raster_data(rasters, output$folder)
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
        # using least_cost method
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

      