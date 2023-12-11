# mo_fill_sinks.R


fill_sinks_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Add a button to open the dialog box to select folder where save dem file
    actionButton(ns("selectFolder"), "Select Folder"),
    
    # Add a radio button for the depression method
    radioButtons(
      ns("depressionMethod"),
      "Select Depression Method",
      choices = list("Wang and Liu" = "wang_liu", "Least Cost" = "least_cost"),
      selected = "wang_liu"
    ),
    
    downloadButton(ns("save_output"), "Save Output"),
    
    leafletOutput(ns("map"), width = "100vw", height = "100vh")
  )
}

fill_sinks_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Open the dialog box when the button is clicked
    observeEvent(input$selectFolder, {
      # Open the dialog box
      output$folder <- renderText({
        folder <- fileDialog(title = "Select Folder",
                             multiple = FALSE,
                             chooseDir = TRUE)
        # Return the selected folder
        folder
      })
    })
    observeEvent(input$dem, {
      req(input$dem)
      # Read the saved raster.tif file
      raster <- terra::rast(paste0(output$folder, "/rasters.tif"))
      # Use the selected depression method
      observeEvent(input$depressionMethod, {
        req(input$depressionMethod)
        if (input$depressionMethod == "wang_liu") {
          # Use the Wang and Liu method
          filled_raster <-
            whitebox::wbt_fill_depressions_wang_and_liu(dem = raster,
                                                        output = "filled_raster.tif")
        } else if (input$depressionMethod == "least_cost") {
          # using least_cost method
          filled_raster <-
            whitebox::wbt_breach_depressions_least_cost(
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
            overlayGroups = c("Fill sinked DEM"),
            options = layersControlOptions(collapsed = FALSE)
          ) %>%
          addRasterImage(filled_raster,
                         group = "Fill sinked DEM",
                         maxBytes = 100 * 1024 ^ 2)
      })
      
      output$save_output <- downloadHandler(
        filename = function() {
          paste(input$save_output,"/filled_raster", ".tif", sep = "")
        },
        content = function(file) {
          if (file.exists("filled_raster.tif")) {
            file.copy("filled_raster.tif", file)
          }
          else {
            stop("the file does not exist.")
          }
        }
      )
    })
    
  })
}
