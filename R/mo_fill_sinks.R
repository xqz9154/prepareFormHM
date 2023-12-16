# fill_sinks.R

# prepare DEM to get rid of pits/depressions, this is done with two steps:
# first fill the single cell depressions, then pass the resulting DEM from the first step to the breach depression function

# UI function
fill_sinks_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # shinyFiles::shinyFileChoose(
    #   ns("selectFolder"),
    #   "Select Folder",
    #   "folder",
    #   multiple = FALSE
    # ),
     
    fileInput(
      ns("dem"),
      "Upload DEM",
      accept = c(".tif", ".geotiff", '.hgt'),
      multiple = FALSE),
    leafletOutput(ns("map"), width = "100vw", height = "100vh"),
    textInput(ns("directory"), "Enter directory for download Filled DEM:", value = getwd()),
    downloadButton(ns("download"), "Download Filled DEM")
    
  )
}

# Server function
fill_sinks_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # selectedFolder <- reactive({
    #   req(input$selectFolder)
    #   input$selectFolder$datapath
    # })
    
    observeEvent(input$dem, {
      req(input$dem)
      ext <- tools::file_ext(input$dem$datapath)
      validate(need(ext %in% c(".tif", ".geotiff", '.hgt'), "Please upload a DEM file."))
      print(paste("File uploaded:", input$dem$datapath))
      if (file.exists(input$dem$datapath)) {
        raster <- raster(input$dem$datapath)
        print(raster)
        output$map <- renderLeaflet({
          leaflet() %>%
            addTiles(group = "Map") %>%
            addLayersControl(
              baseGroups = c("Map"),
              overlayGroups = c("DEM"),
              options = layersControlOptions(collapsed = FALSE)
            ) %>%
            leafem::addGeoRaster(raster,
                                 group = "DEM") 
        })
      }
      
      filled_raster <- reactive({
        # output_path = paste0(selectedFolder(), "/output/")
        output_path = input$directory
        if (!dir.exists(output_path) ){
          dir.create(output_path)
        }
        # first breach depressions using wbt_breach_depressions_least_cost(), 
        # which will lower the elevation of the cells damming depression
        tryCatch({
          breached_dem <- whitebox::wbt_breach_depressions_least_cost(
            dem = raster,
            output = paste0(output_path, "breached_dem.tif"),
            dist = 5,
            fill = TRUE,
            wd = getwd(),
            verbose_mode = TRUE
          )
        }, error = function(e) {
          print(paste0("Error: wbt_breach_depressions_least_cost", e))
        })
        # second use wbt_fill_depressions_wang_and_liu() to clean up any remaining issues
        # Be careful to give wbt_fill_depressions_wang_and_liu() the result of the breach depressions function
        tryCatch({
          whitebox::wbt_fill_depressions_wang_and_liu(
            dem = paste0(output_path, "breached_dem.tif"),
            output = paste0(output_path, "filled_breached_dem.tif"),
            wd = getwd(),
            verbose_mode = TRUE
          )
        }, error = function(e) {
          print(paste0("Error: wbt_fill_depressions_wang_and_liu", e))
        })
      
        return(paste0(output_path, "filled_breached_dem.tif")) # Return output_path
      })
      
      observeEvent(filled_raster(), {
        if (is.null(filled_raster())) {
          showModal(modalDialog(
            title = "Error",
            "There was an error filling the depressions."
          ))
        } else {
          output$map <- renderLeaflet({
            createMap(raster, filled_raster())
          })
        }
      })
      
      
      observeEvent(input$download, {
        if (!is.null(filled_raster())) {
          download.file(
            paste0(filled_raster(), "filled_dem.tif"),
            "filled_dem.tif",
            method = "auto"
          )
        }
      })
    })
  })
}

createMap <- function(raster, output_path) {
  leaflet() %>%
    addTiles(group = "Map") %>%
    addLayersControl(
      baseGroups = c("Map"),
      overlayGroups = c("DEM", "Fill sinked DEM"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    leafem::addGeoRaster(raster, group = "DEM") %>%
    leafem::addGeoRaster(raster(paste0(output_path, "filled_breached_dem.tif")), group = "Fill sinked DEM")
}

