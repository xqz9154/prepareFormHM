# main.R
source("load_library.R")
source("upload_display.R")
source("draw_polygon.R")
source("downloadDEM.R")

# 
ui <- navbarPage(
  "Download DEM for study area" ,
  tabPanel("Draw a polygon", draw_polygon_ui("draw_polygon")),
  tabPanel("Upload a Shapefile", upload_display_ui("upload_display"))  ,
  tabPanel("Download DEM", downDEM_ui("downloadDEM"))
  
)


server <- function(input, output, session) {
  drawnPolygon <- reactiveValues(drawnPolygon = NULL)
  uploadedShapefile <- reactiveValues(value = NULL)
  
  draw_polygon_server("draw_polygon", drawnPolygon)
  upload_display_server("upload_display", uploadedShapefile)
  downloadDEM_server("downloadDEM",uploadedShapefile)
}

shinyApp(ui = ui, server = server)
