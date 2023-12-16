# pour_points.R
# using wbt_jenson_snap_pour_points to move the pour points to the closest stream over a defined distance
# two inputs: pour points and stream network
# pour points could be drawn from map or upload by user as csv or shapefile
# stream network could be extract from flow accumulation file created in mo_facc.R according to user defined threshold

# UI
pour_points_ui <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map")),
    # fileInput(ns("streams"), )
    downloadButton(ns("download"))
  )
}

# # Server
pour_points_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles()
    })

    observeEvent(input$map_marker_click, {
      req(input$map_marker_click)
       

      # snap pour points to the closest stream
      wbt_jenson_snap_pour_points(
        pour_pts = input$map_marker_click,
        streams = paste0(output_path,"streams.tif"),
        output = paste0(output_path,"pour_points.tif"),
        snap_dist = 1000, # adjust this value as needed
        wd = getwd(),
        verbose_mode = TRUE
      )
    })

    output$download <- downloadHandler(
      filename = function() {
        paste("pour_points", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(input$map_marker_click, file)
      }
    )
  })
}
 
