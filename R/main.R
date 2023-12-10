# main.R
source("load_library.R")
source("upload_display.R")
source("draw_polygon.R")
source("downloadDEM.R")
source("mo_DEM_preprocess.R")
source("mo_fill_sinks.R")



# 
ui <- navbarPage(
  "Download DEM for study area" ,
  tabPanel("Draw a polygon", draw_polygon_ui("draw_polygon")),
  tabPanel("Upload a Shapefile", upload_display_ui("upload_display")),
  tabPanel("Download DEM", downloadDEM_ui("downloadDEM")),
  tabPanel("DEM preprocess",DEM_preprocess_ui("DEM_preprocess")),
  tabPanel("Fill sinks",fill_sinks_ui("Fill sinks"))
)


server <- function(input, output, session) {
  drawnPolygon <- reactiveValues(drawnPolygon = NULL)
  uploadedShapefile <- reactiveValues(value = NULL)
  
  draw_polygon_server("draw_polygon", drawnPolygon)
  upload_display_server("upload_display", uploadedShapefile)
  downloadDEM_server("downloadDEM",uploadedShapefile)
  DEM_preprocess_server("DEM_preprocess")
  fill_sinks_server("Fill sinks")
  
}

shinyApp(ui = ui, server = server)
