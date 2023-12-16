# draw_polygon.R
draw_polygon_ui <- function(id) {
  ns <- NS(id)
  download_shp <- ns('download_shp')
  tagList(leafletOutput(ns("mymap")))
  
}

draw_polygon_server <- function(id, drawnPolygon) {
  moduleServer(id, function(input, output, session) {
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addDrawToolbar()
    })
    observeEvent(input$mymap_draw_new_feature, {
      coords <- input$mymap_draw_new_feature$geometry$coordinates
      points <- unlist(coords, recursive = TRUE)
      mtx <- matrix(points, ncol = 2, byrow = TRUE)
      polygon <- st_polygon(list(mtx)) %>% st_sfc(crs = 4326)
      drawnPolygon$drawnPolygon <- polygon
    })
    
    # Write the shapefile to a specific location
    temp_shp <- getwd()
    temp_file <- paste0(temp_shp, "/temp_shp")
    
    # Check if the directory exists and create it if it doesn't
    if (!dir.exists(temp_file)) {
      dir.create(temp_file)
    }
    
    # Access the reactive value inside a reactive context
    observe({
      if (!is.null(drawnPolygon$drawnPolygon)) {
        st_write(drawnPolygon$drawnPolygon, temp_file,append=FALSE, driver = "ESRI Shapefile")
      }
    })
    
  })
}
