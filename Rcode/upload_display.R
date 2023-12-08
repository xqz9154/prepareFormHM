# upload_display.R
upload_display_ui <- function(id) {
  ns <- NS(id)
  tagList(fileInput(
    ns("shapefile"),
    "Upload Shapefile",
    accept = c('.shp', '.dbf', '.sbn', '.sbx', '.shx', '.prj'),
    multiple = TRUE
  ),
  leafletOutput(ns("mymap")))
}


# upload_display.R
upload_display_server <- function(id, uploadedShapefile) {
  moduleServer(id, function(input, output, session) {
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
    # display the map
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addTiles()
    })
    # update the map
    observeEvent(uploadedShapefile$value, {
      output$mymap <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          addPolygons(data = uploadedShapefile$value)
      })
    })
  })
}
