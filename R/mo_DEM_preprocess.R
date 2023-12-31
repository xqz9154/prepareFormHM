# DEM preprocess

DEM_preprocess_ui <- function(id) {
  ns <- NS(id)
  
  tagList(fileInput(
    ns("dem") ,
    "Select DEM Dataset",
    accept = c(".tif", ".geotiff", '.hgt'),
    multiple = TRUE
  ),
  leafletOutput(ns("map")))
}

DEM_preprocess_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$dem, {
      req(input$dem)
      rasters <- list()
      
      # print(seq_along(input$dem))
      tryCatch(
        rasters <- lapply(input$dem$datapath, function(x) {
          file_ext <- tools::file_ext(x)
          if (file_ext %in% c("tif", "hgt", "geotiff")) {
            return(raster::raster(x))
          } else {
            stop("Invalid file format. Please upload tif, hgt or geotiff files.")
          }
        })
        ,
        error = function(e) {
          message('An Error Occurred at uploading DEM file')
          print(e)
        }
      )
      
      print(rasters)
      
   
      # If there are multiple raster files selected, merge them together
      if (length(rasters) > 1) {
        merge_rasters <- do.call(terra::merge, rasters)
        
        # resample to lower resolution
        fact <-
          round(dim(merge_rasters)[1:2] / dim(rasters[[1]])[1:2])
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
        
      } else
        #there is only one raster file selected
      {
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
              addRasterImage(rasters[[1]],
                             group = "Raster",
                             maxBytes = 100 * 1024 ^ 2)
          })
        })
      }
    })
  })
}



# # Fill single cell sinks then breach larger sinks
# wbt_breach_depressions_least_cost(
#   dem = input$dem$datapath,
#   output = here('./Output/dem_clip_fill_breached.tif'),
#   dist = 5,
#   fill = TRUE
# )
# wbt_fill_depressions_wang_and_liu(
#   dem = here('./output/dem_clip_fill_breached.tif'),
#   output = here('./output/dem_clip_fill_breached_wang_liu.tif')
# )
#
# # Create D8 flow accumulation and D8 pointer grids
# wbt_d8_flow_accumulation(
#   input = here('./Output/dem_clip_fill_breached_wang_liu.tif'),
#   output = here('./Output/dem_clip_D8FA.tif')
# )
# wbt_d8_pointer(
#   dem = here('./Output/dem_clip_fill_breached_wang_liu.tif'),
#   output = here('./Output/dem_clip_D8pt.tif')
# )
#
# # Extract streams
# wbt_extract_streams(
#   flow_accum = here('./Output/dem_clip_D8FA.tif'),
#   output = here('./Output/raster_streams.tif'),
#   threshold = 5000
# )
# wbt_raster_streams_to_vector(
#   streams = here('./Output/raster_streams.tif'),
#   d8_pntr = here('./Output/dem_clip_D8pt.tif'),
#   output = here('./Output/streams_6000.shp')
# )
#
# # Run watershed function
# wbt_watershed(
#   pour_points = input$pour_points$datapath,
#   d8_pntr = here('./Output/dem_clip_D8pt.tif'),
#   output = here('./Output/watershed.shp')
# )

# Display the result
# output$map <- renderLeaflet({
#   leaflet() %>%
#     (st_geometry(read_sf(here(
#       './Output/watershed.shp'
#     ))))
# })