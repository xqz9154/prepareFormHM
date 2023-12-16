# mo_create_stream.R


#' Title
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
create_streams_ui <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map")),
     numericInput(ns("threshold"), "Enter threshold value", value = 6000)
    )
}

#' Title
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
create_streams_server <-   function(id,facc_path) {
  moduleServer(id, function(input, output, session) {
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles()
    })
    
    observeEvent(facc_path(), {
      req(facc_path())
      print(facc_path())
      # read flow accumulation
      facc <- raster(facc_path())
      print(facc)
       
      # create stream network using user defined threshold
      wbt_extract_streams(
        flow_accum = facc,
        output = paste0(getwd(), "/output/", "streams.tif"),
        threshold = input$threshold
      )
      
      # # update the map to add streams
      streams <- raster(paste0(getwd(), "/output/", "streams.tif"))
      output$map <- leaflet() %>%
        addTiles() %>% 
        addRasterImage(streams)
    })
  })
}