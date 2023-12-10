# download  and crop DEM using uploaded shapefile

#
downloadDEM_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("source"), "Select DEM Source", choices = c("aws", "gl3", "gl1", "alos", "srtm15plus")),
    downloadButton(ns("downloadData"), "Download DEM"),
    textInput(ns("filename"), "Enter filename for DEM:", value = "cropped_dem")
  )
}


#
downloadDEM_server <- function(id, uploadedShapefile) {
  moduleServer(id, function(input, output, session) {
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$filename, ".tif", sep = "")
      },
      content = function(file) {
        bbox <- if (any(!is.null(uploadedShapefile$value))) {
          uploadedShapefile$value
        } else {
          print("Please update a valid shapefile!!!")
        }
        dem <-
          elevatr::get_elev_raster(bbox, source = input$source, z = 10)
        cropped_dem <- raster::crop(dem, bbox)
        raster::writeRaster(cropped_dem, file, format = "GTiff")
      }
    )
  })
}