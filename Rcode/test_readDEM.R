# test download drawn polygon on leaflet and save to local folder as shapefile


# drawing polygon and save to local folder -------
drawnPolygon <- reactiveValues(drawnPolygon = NULL)
observeEvent(input$mymap_draw_new_feature, {
  coords <- input$mymap_draw_new_feature$geometry$coordinates
  points <- unlist(coords, recursive = TRUE)
  mtx <- matrix(points, ncol = 2, byrow = TRUE)
  polygon <- st_polygon(list(mtx)) %>% st_sfc(crs = 4326)
  drawnPolygon$drawnPolygon <- polygon
  # print(class(drawnPolygon$drawnPolygon))
})
# observeEvent(input$savePolygon, {
#   # Write the drawn polygon to a shapefile
#   sf::st_write(drawnPolygon$drawnPolygon, "drawn_polygon.shp")
# })
# # save the drawn polygon to local folder
output$savePolygon <- downloadHandler(
  filename = function() {
    # paste("polygon", Sys.Date(), ".geojson", sep="")
    paste("polygon", Sys.Date(), ".shp", sep = "")
  },
  content = function(file) {
    #     st_write(drawnPolygon$drawnPolygon, file, driver = "GeoJSON")
    sf::st_write(drawnPolygon$drawnPolygon, file)
  }
)